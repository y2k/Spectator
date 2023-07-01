#r "nuget: SodiumFRP.FSharp, 5.0.6"

module Utils =
    open Sodium.Frp

    let inline accum f empty e =
        e |> Stream.accum empty (fun a b -> f b a)

    let inline snapshot cell f stream =
        stream |> Stream.snapshot cell (fun a b -> f b a)

    let inline merge a b = Stream.merge (fun x _ -> x) (a, b)

    [<CompilerMessage("Incomplete hole", 130)>]
    let inline FIXME (x: _) = raise (exn $"Incomplete hole: {x}")

open Utils

module Types =
    type Snapshot = { url: string; subId: string }

    type Subscription =
        { id: string
          user: string
          url: string }

    type NewSubscription =
        { id: string
          user: string
          url: string }

    type SubscriptionChanged =
        | NewSubscriptionCreated of NewSubscription
        | NewSubscriptionRemoved of string
        | SubscriptionCreated of Subscription
        | SubscriptionRemoved of string

    type SnapshotCreated = SnapshotCreated of Snapshot

    type DownloadEffect = DownloadEffect of url: string * (byte array -> obj)
    type TelegramMessageReceived = TelegramMessageReceived of user: string * message: string
    type TelegramMessageSended = TelegramMessageSended of user: string * message: string

open Types

module TelegramBot =
    type State = { subs: Map<string, string list> }

    let _listenSubscriptionChanged (state: State) =
        function
        | SubscriptionCreated e -> { state with subs = FIXME e state.subs }
        | SubscriptionRemoved e -> { state with subs = FIXME e state.subs }
        | _ -> state

    let _listenTelegramMessages (state: State) (TelegramMessageReceived(user, message)) : obj list =
        match message.Split ' ' with
        | [| "/add"; url |] ->
            let ns: NewSubscription = FIXME url user
            [ TelegramMessageSended(user, "Url added"); NewSubscriptionCreated ns ]
        | [| "/ls" |] ->
            let message =
                state.subs
                |> Map.tryFind user
                |> Option.defaultValue []
                |> List.fold (sprintf "%s\n- %s") "Your subs: "

            [ TelegramMessageSended(user, message) ]
        | _ -> [ TelegramMessageSended(user, "Unknown message") ]

module SubWorker =
    type State = { newSubs: string Set }
    type DownloadComplete = DownloadComplete of string * byte array

    let _listenSubscriptionChanged (state: State) =
        function
        | NewSubscriptionCreated e ->
            { state with
                newSubs = Set.add e.id state.newSubs }
        | NewSubscriptionRemoved id ->
            { state with
                newSubs = Set.remove id state.newSubs }
        | _ -> state

    let _listenDownloadCompleted (state: State) (DownloadComplete(newSubId, data)) : obj list =
        let newSubIsExist = state.newSubs |> Set.contains newSubId
        let sub: Subscription = FIXME data

        if newSubIsExist then
            [ NewSubscriptionRemoved newSubId; SubscriptionCreated sub ]
        else
            []

    let _listenNewSubscriptions (state: State) e : obj list =
        match e with
        | NewSubscriptionCreated e ->
            [ DownloadEffect(e.url, (fun data -> DownloadComplete(e.id, data)))
              { state with
                  newSubs = state.newSubs |> Set.add e.id } ]
        | _ -> []

module SnapshotWorker =
    type State = { subs: Subscription list }
    type DownloadComplete = DownloadComplete of data: byte array * subId: string

    let _listenSubscriptionCreated (state: State) =
        function
        | SubscriptionCreated e -> { state with subs = e :: state.subs }
        | SubscriptionRemoved subId ->
            { state with
                subs = state.subs |> List.filter (fun x -> x.id <> subId) }
        | _ -> state

    let _listenDownloadCompleted (state: State) (DownloadComplete(data, subId)) : obj list =
        let subExists = state.subs |> List.exists (fun x -> x.id = subId)

        if subExists then
            let snap: Snapshot = FIXME data
            [ SnapshotCreated snap ]
        else
            []

    let _listerTimerTicked (state: State) : obj list =
        let subs: Subscription list = FIXME state.subs

        subs
        |> List.map (fun sub -> DownloadEffect(sub.url, (fun data -> DownloadComplete(data, sub.id))))

module Notifications =
    type State = { subscriptions: Map<string, string> }

    let _listenSubscriptionChanged (state: State) =
        function
        | SubscriptionCreated e ->
            { state with
                subscriptions = state.subscriptions |> Map.add e.id e.user }
        | SubscriptionRemoved e ->
            { state with
                subscriptions = state.subscriptions |> Map.remove e }
        | _ -> state

    let _listenSnapshotCreated (state: State) (SnapshotCreated e) : obj list =
        let user = state.subscriptions |> Map.find e.subId
        [ TelegramMessageSended(user, $"Snapshot created: {e.url}") ]

module Application =
    open Sodium.Frp

    let () =
        let snapCreatedEvent = StreamSink.create<SnapshotCreated> ()
        let subChangedEvent = StreamSink.create<SubscriptionChanged> ()
        let telegramMessageReceived = StreamSink.create<TelegramMessageReceived> ()

        let _a =
            let state =
                subChangedEvent |> accum SnapshotWorker._listenSubscriptionCreated { subs = [] }

            let _a =
                FIXME ""
                |> snapshot state (fun state _ -> SnapshotWorker._listerTimerTicked state)

            let downloadComplete = StreamSink.create<SnapshotWorker.DownloadComplete> ()
            let _b = downloadComplete |> snapshot state SnapshotWorker._listenDownloadCompleted
            merge _a _b

        let _b =
            let state =
                subChangedEvent
                |> accum SubWorker._listenSubscriptionChanged { newSubs = Set.empty }

            let _a = subChangedEvent |> snapshot state SubWorker._listenNewSubscriptions

            let downloadComplete = StreamSink.create<SubWorker.DownloadComplete> ()

            let _b = downloadComplete |> snapshot state SubWorker._listenDownloadCompleted
            merge _a _b

        let _c =
            let state =
                subChangedEvent
                |> accum TelegramBot._listenSubscriptionChanged { subs = Map.empty }

            telegramMessageReceived |> snapshot state TelegramBot._listenTelegramMessages

        let _d =
            let state =
                subChangedEvent
                |> accum Notifications._listenSubscriptionChanged { subscriptions = Map.empty }

            snapCreatedEvent |> snapshot state Notifications._listenSnapshotCreated

        let _effects = merge (merge _a _b) (merge _c _d)

        ()
