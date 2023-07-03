#r "nuget: SodiumFRP.FSharp, 5.0.6"

open System

module Utils =
    open Sodium.Frp

    let inline merge a b = Stream.merge (fun x _ -> x) (a, b)

    [<CompilerMessage("Incomplete hole", 130)>]
    let inline FIXME (x: _) = raise (exn $"Incomplete hole: {x}")

open Utils

module Types =
    type Snapshot =
        { url: string
          subId: string
          data: byte array }

    type Subscription =
        { id: string
          user: string
          url: string
          data: byte array }

    type NewSubscription =
        { id: string
          user: string
          url: string }

    type SubscriptionChanged =
        | NewSubscriptionCreated of NewSubscription
        | NewSubscriptionRemoved of string
        | SubscriptionCreated of Subscription
        | SubscriptionRemoved of user: string * id: string

    type SnapshotCreated = SnapshotCreated of Snapshot

    type DownloadRequested = DownloadRequested of url: string * (byte array -> obj)
    type TelegramMessageReceived = TelegramMessageReceived of user: string * message: string
    type TelegramMessageSended = TelegramMessageSended of user: string * message: string
    type TimeTicked = TimeTicked
    type EventDispatched = EventDispatched of event: obj

open Types

module TelegramBot =
    type State =
        { userSubs: Map<string, {| id: string; url: string |} list> }

    let _listenSubscriptionChanged (state: State) =
        function
        | SubscriptionCreated e ->
            { state with
                userSubs =
                    state.userSubs
                    |> Map.tryFind e.user
                    |> Option.defaultValue []
                    |> fun urls -> Map.add e.user ({| id = e.id; url = e.url |} :: urls) state.userSubs }
        | SubscriptionRemoved(user, sid) ->
            { state with
                userSubs =
                    state.userSubs
                    |> Map.tryFind user
                    |> Option.defaultValue []
                    |> fun subs -> Map.add user (subs |> List.filter (fun u -> u.id <> sid)) state.userSubs }
        | _ -> state

    let _listenTelegramMessages (state: State) (TelegramMessageReceived(user, message)) : obj list =
        match message.Split ' ' with
        | [| "/add"; url |] ->
            let ns =
                { id = Guid.NewGuid().ToString()
                  user = user
                  url = url }

            [ TelegramMessageSended(user, "Url added")
              EventDispatched(NewSubscriptionCreated ns) ]
        | [| "/ls" |] ->
            let message =
                state.userSubs
                |> Map.tryFind user
                |> Option.defaultValue []
                |> List.map (fun x -> x.url)
                |> List.fold (sprintf "%s\n- %s") "Your subs: "

            [ TelegramMessageSended(user, message) ]
        | _ -> [ TelegramMessageSended(user, "Unknown message") ]

module SubWorker =
    type State = { newSubs: string Set }
    type DownloadComplete = DownloadComplete of newSubId: string * user: string * url: string * byte array

    let _listenSubscriptionChanged (state: State) =
        function
        | NewSubscriptionCreated e ->
            { state with
                newSubs = Set.add e.id state.newSubs }
        | NewSubscriptionRemoved id ->
            { state with
                newSubs = Set.remove id state.newSubs }
        | _ -> state

    let _listenDownloadCompleted (state: State) (DownloadComplete(newSubId, user, url, data)) : obj list =
        let newSubIsExist = state.newSubs |> Set.contains newSubId

        if newSubIsExist then
            let sub: Subscription =
                { id = newSubId
                  user = user
                  url = url
                  data = data }

            [ EventDispatched(NewSubscriptionRemoved newSubId)
              EventDispatched(SubscriptionCreated sub) ]
        else
            []

    let _listenNewSubscriptions e : obj list =
        match e with
        | NewSubscriptionCreated e ->
            [ DownloadRequested(e.url, (fun data -> DownloadComplete(e.id, e.user, e.url, data))) ]
        | _ -> []

module SnapshotWorker =
    type State = { subs: Subscription list }
    type DownloadComplete = DownloadComplete of data: byte array * subId: string * url: string

    let _listenSubscriptionCreated (state: State) =
        function
        | SubscriptionCreated e -> { state with subs = e :: state.subs }
        | SubscriptionRemoved(_, subId) ->
            { state with
                subs = state.subs |> List.filter (fun x -> x.id <> subId) }
        | _ -> state

    let _listenDownloadCompleted (state: State) (DownloadComplete(data, subId, url)) : obj list =
        let subExists = state.subs |> List.exists (fun x -> x.id = subId)

        if subExists then
            let snap: Snapshot =
                { url = url
                  subId = subId
                  data = data }

            [ EventDispatched(SnapshotCreated snap) ]
        else
            []

    let _listerTimerTicked (state: State) : obj list =
        let subs: Subscription list = state.subs

        subs
        |> List.map (fun sub -> DownloadRequested(sub.url, (fun data -> DownloadComplete(data, sub.id, sub.url))))

module Notifications =
    type State =
        { subscriptions: Map<string, {| user: string |}> }

    let _listenSubscriptionChanged (state: State) =
        function
        | SubscriptionCreated e ->
            { state with
                subscriptions = state.subscriptions |> Map.add e.id {| user = e.user |} }
        | SubscriptionRemoved(_, id) ->
            { state with
                subscriptions = state.subscriptions |> Map.remove id }
        | _ -> state

    let _listenSnapshotCreated (state: State) (SnapshotCreated e) : obj list =
        let x = state.subscriptions |> Map.find e.subId
        [ TelegramMessageSended(x.user, $"Snapshot created: {e.url}") ]

module Application =
    open Sodium.Frp

    type Meta =
        { level: int
          id: Guid }

        static member empty() = { level = 0; id = Guid.NewGuid() }

    let () =
        let snapCreatedEvent = StreamSink.create<Meta * SnapshotCreated> ()
        let subChangedEvent = StreamSink.create<Meta * SubscriptionChanged> ()
        let telegramMessageReceived = StreamSink.create<Meta * TelegramMessageReceived> ()
        let timeEvent = StreamSink.create<Meta * TimeTicked> ()

        let snapDownloadComplete =
            StreamSink.create<Meta * SnapshotWorker.DownloadComplete> ()

        let subDownloadComplete = StreamSink.create<Meta * SubWorker.DownloadComplete> ()

        let inline accum f empty e =
            e |> Stream.accum empty (fun (_, a) b -> f b a)

        let inline snapshot cell f stream =
            stream |> Stream.snapshot cell (fun (meta, a) b -> meta, f b a)

        let _a =
            let state =
                accum SnapshotWorker._listenSubscriptionCreated { subs = [] } subChangedEvent

            merge
                (snapshot state (fun state _ -> SnapshotWorker._listerTimerTicked state) timeEvent)
                (snapshot state SnapshotWorker._listenDownloadCompleted snapDownloadComplete)

        let _b =
            let state =
                accum SubWorker._listenSubscriptionChanged { newSubs = Set.empty } subChangedEvent

            merge
                (snapshot state (fun _ e -> SubWorker._listenNewSubscriptions e) subChangedEvent)
                (snapshot state SubWorker._listenDownloadCompleted subDownloadComplete)

        let _c =
            let state =
                accum TelegramBot._listenSubscriptionChanged { userSubs = Map.empty } subChangedEvent

            snapshot state TelegramBot._listenTelegramMessages telegramMessageReceived

        let _d =
            let state =
                accum Notifications._listenSubscriptionChanged { subscriptions = Map.empty } subChangedEvent

            snapshot state Notifications._listenSnapshotCreated snapCreatedEvent

        (* APPLICATION *)

        let log meta type' e =
            printfn "%s[%s] %O" (String.replicate meta.level "  ") type' e

        let send meta e s =
            log meta "EVENT" e
            StreamSink.send (meta, e) s

        merge (merge _a _b) (merge _c _d)
        |> Stream.listen (fun (meta, fxs) ->
            let meta = { meta with level = meta.level + 1 }

            fxs
            |> List.iter (fun fx ->
                log meta "FX" fx

                match fx with
                | :? EventDispatched as (EventDispatched ed) ->
                    match ed with
                    | :? SnapshotCreated as sc ->
                        Transaction.post (fun _ ->
                            let meta = { meta with level = meta.level + 1 }
                            send meta sc snapCreatedEvent)
                    | :? SubscriptionChanged as fx ->
                        Transaction.post (fun _ ->
                            let meta = { meta with level = meta.level + 1 }
                            send meta fx subChangedEvent)
                    | ed -> failwithf "Unknown event: %O" ed
                | :? DownloadRequested as DownloadRequested(_, cb) ->
                    Transaction.post (fun _ ->
                        let meta = { meta with level = meta.level + 1 }

                        match cb [||] with
                        | :? SnapshotWorker.DownloadComplete as re -> send meta re snapDownloadComplete
                        | :? SubWorker.DownloadComplete as re -> send meta re subDownloadComplete
                        | re -> failwithf "Unknown callback %O" re)
                | :? TelegramMessageSended -> ()
                | fx -> failwithf "Unknown effect: %O" fx))
        |> ignore

        printfn "================================================================\n"
        send (Meta.empty ()) (TelegramMessageReceived("y2k", "/add https://g.com/")) telegramMessageReceived
        printfn "\n================================================================\n"
        send (Meta.empty ()) (TelegramMessageReceived("y2k", "/ls")) telegramMessageReceived
        printfn "\n================================================================\n"
        send (Meta.empty ()) TimeTicked timeEvent
