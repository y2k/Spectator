module Spectator.Bot.App

open System
open Spectator.Core

module Parser =
    type Cmd =
        | History of Uri
        | GetUserSubscriptionsCmd
        | UnknownCmd
        | AddNewSubscriptionCmd of Uri * string option
        | DeleteSubscriptionCmd of Uri

    let isValidRegex pattern =
        if String.IsNullOrEmpty pattern then false
        else
            try
                Text.RegularExpressions.Regex.Match("", pattern) |> ignore
                true
            with
            _ -> false

    let parse (message : string) =
        let isValidUri url = Uri.IsWellFormedUriString(url, UriKind.Absolute)
        match message with
        | Regex "/ls" [] -> GetUserSubscriptionsCmd
        | Regex "/add ([^ ]+) ([^ ]+)" [ url; filter ] when isValidUri url && isValidRegex filter ->
            AddNewSubscriptionCmd (Uri url, Some filter)
        | Regex "/add ([^ ]+)" [ url ] when isValidUri url -> AddNewSubscriptionCmd (Uri url, None)
        | Regex "/rm ([^ ]+)" [ url ] when isValidUri url -> DeleteSubscriptionCmd @@ Uri url
        | Regex "/history ([^ ]+)" [ url ] when isValidUri url -> History @@ Uri url
        | _ -> UnknownCmd

module Domain =
    let snapshotsCount (snapshots : Snapshot list) subId =
        snapshots
        |> List.filter @@ fun sn -> sn.subscriptionId = subId
        |> List.length

    let subToString snapshots (sub : Subscription) =
        let provider =
            match sub.provider with
            | id when id = Guid "E5D3A9F2-325C-4CEF-BCA9-99D23F9E5AE5" -> "RSS"
            | id when id = Guid "3B26457E-8AB7-41AD-8DEC-11AF891A3052" -> "Telegram"
            | id when id = Guid "AE4FEE1F-C08D-44B9-B526-4162FF1C328C" -> "HTML"
            | id -> sprintf "Unknown (%s)" (id.ToString().Substring(0, 4))
        sprintf "[%s] %O '%s' (%i)" provider sub.uri sub.filter (snapshotsCount snapshots sub.id)

    let subListToMessageResponse (subscriptions : Subscription list) newSubscriptions userId snapshots =
        let subs =
            subscriptions
            |> List.filter ^ fun x -> x.userId = userId
            |> List.map (subToString snapshots)
        newSubscriptions
        |> List.filter ^ fun x -> x.userId = userId
        |> List.map ^ fun x -> sprintf "[Processing...] %O '%s'" x.uri x.filter
        |> List.append subs
        |> List.fold (sprintf "%s\n- %s") "Your subscriptions: "

    let deleteSubs (subscriptions : Subscription list) newSubscriptions userId uri =
        newSubscriptions |> List.filter ^ fun x -> x.userId <> userId || x.uri <> uri
        , subscriptions |> List.filter ^ fun x -> x.userId <> userId || x.uri <> uri

    let createNewSub user uri filter =
        { id = TypedId.wrap <| Guid.NewGuid (); userId = user; uri = uri; filter = Option.defaultValue "" filter }

    let add newSubscriptions user uri filter =
        let sub = createNewSub user uri filter
        sub :: newSubscriptions

    let getHistory (snapshots : Snapshot list) (subs : Subscription list) userId uri =
        subs
        |> List.tryFind @@ fun sub -> sub.uri = uri && sub.userId = userId
        |> function
           | None -> "Not found subscription with this url"
           | Some sub ->
               snapshots
               |> List.filter @@ fun sn -> sn.subscriptionId = sub.id
               |> List.fold (fun a x -> sprintf "%s\n- %O" a x.uri) "History:"

    let removeSubs (subscriptions : Subscription list) (newSubscriptions : NewSubscription list) (ns : NewSubscription list) (ss : Subscription list) =
        let remSubs =
            subscriptions
            |> List.map @@ fun x -> x.id
            |> List.filter @@ fun id ->
                ss |> List.forall @@ fun si -> si.id <> id
        let rmNewSubs =
            newSubscriptions
            |> List.map @@ fun x -> x.id
            |> List.filter @@ fun id ->
                ns |> List.forall @@ fun nsi -> nsi.id <> id
        remSubs, rmNewSubs

module Store =
    type UserState =
        { subscriptions : Subscription list
          newSubscriptions : NewSubscription list
          snapshots : Snapshot list }

    type State = { states : Map<UserId, UserState> }

    let updateUserState (state : State) (userId : UserId) (f : UserState -> UserState * _) : State * _ =
        let us =
            Map.tryFind userId state.states
            |> Option.defaultValue { snapshots = []; subscriptions = []; newSubscriptions = [] }
        let (us, effs) = f us
        { state with states = Map.add userId us state.states }, effs

module StoreUpdater =
    open Store

    let init = { states = Map.empty }

    let private updateUserStateSafe state userId f =
        updateUserState state userId (fun s -> f s, []) |> fst

    let private findUserBySnapshot (state : State) (snap : Snapshot) : UserId option =
        state.states
        |> Map.tryFindKey @@ fun _ v ->
            v.subscriptions
            |> List.exists @@ fun sub -> sub.id = snap.subscriptionId

    let private removeSubsWithIds states ids =
        states
        |> Map.map @@ fun _ state ->
            { state with
                newSubscriptions =
                    state.newSubscriptions
                    |> List.filter (fun s -> not <| List.contains s.id ids) }

    let update state = function
        | SnapshotCreated snap ->
            match findUserBySnapshot state snap with
            | Some userId ->
                updateUserStateSafe state userId @@ fun us ->
                    let snaps = snap :: us.snapshots
                    { us with snapshots = snaps |> List.take (min 10 snaps.Length) }
            | None -> state
        | SubscriptionCreated sub ->
            updateUserStateSafe state sub.userId @@ fun us ->
                { us with subscriptions = sub :: us.subscriptions }
        | SubscriptionRemoved (_, ids) ->
            { state with states = removeSubsWithIds state.states ids }
        | NewSubscriptionCreated ns ->
            updateUserStateSafe state ns.userId @@ fun us ->
                { us with newSubscriptions = ns :: us.newSubscriptions }

module HandleTelegramMessage =
    open Store
    module D = Domain
    module P = Parser

    let invoke (user : UserId) text (db : UserState) =
        match P.parse text with
        | P.History url ->
            // db
            ((user, D.getHistory db.snapshots db.subscriptions user url)
            , [])
        | P.GetUserSubscriptionsCmd ->
            // db
            ((user, D.subListToMessageResponse db.subscriptions db.newSubscriptions user db.snapshots)
            , [])
        | P.DeleteSubscriptionCmd uri ->
            // let (ns, ss) = D.deleteSubs db.subscriptions db.newSubscriptions user uri
            let (remSubs, rmNewSubs) =
                D.deleteSubs db.subscriptions db.newSubscriptions user uri
                ||> D.removeSubs db.subscriptions db.newSubscriptions
            // { db with subscriptions = ss; newSubscriptions = ns }
            ((user, "Your subscription deleted")
            , [ SubscriptionRemoved (remSubs, rmNewSubs) ])
        | P.AddNewSubscriptionCmd (uri, filter) ->
            let newSub = D.createNewSub user uri filter
            // { db with newSubscriptions = newSub :: db.newSubscriptions }
            ((user, "Your subscription created")
            , [ NewSubscriptionCreated newSub ])
        | P.UnknownCmd ->
            // db
            ((user, "/ls - Show your subscriptions\n/add [url] - Add new subscription\n/rm [url] - Add new subscription\n/history [url] - show last snapshots for subscription with url")
            , [])

// module Updater2 =
//     open Store

//     let update sendEvent sendMessage state (userId, message) =
//         printfn "Telegram (%s) :: %s" userId message
//         let state, (r, cmd) = updateUserState state userId (HandleTelegramMessage.invoke userId message)
//         state, sendMessage r :: (cmd |> List.map sendEvent)

module Updater =
    open Store

    // type Msg =
    //     | EventsReceived of Events
    //     | TextReceived of UserId * string
    //     | SendMessageEnd
    //     | SendEventEnd

// type Cmd =
// | ReadNewMessage of (UserId * string -> Msg)
// | SendEvent of Events
// | SendMessage of UserId * string

    // let main readMessage sendEvent sendMessage state =
    //     readMessage (fun (user, msg) ->
    //         let (_s, xs) = Updater2.update sendEvent sendMessage state (user, msg)
    //         xs)

    // let update sendEvent sendMessage state (userId, message) =
    //     printfn "Telegram (%s) :: %s" userId message
    //     let state, (r, cmd) = updateUserState state userId (HandleTelegramMessage.invoke userId message)
    //     state, sendMessage r :: (cmd |> List.map sendEvent)

    let main readMessage sendMessage updateState =
        async {
            let! (user, msg) = readMessage

            let! db =
                updateState @@ fun db ->
                    let (a, b) = snd <| updateUserState db user (fun db -> db, HandleTelegramMessage.invoke user msg db)
                    db, b

            let telegramMsg =
                fst @@ snd @@ updateUserState db user (fun db -> db, HandleTelegramMessage.invoke user msg db)

            let! _ = telegramMsg ||> sendMessage
            return ()
        }

let emptyState = StoreUpdater.init

let restore = StoreUpdater.update

let main = Updater.main

    // let runMain (readEvent : 'e Async) (sendEvent : 'e -> unit Async) (initState : 's) updateStore f =
    //     let state = ref initState
    //     let syncState : unit Async =
    //         async {
    //             while true do
    //                 let! e = readEvent
    //                 state := updateStore !state e
    //         }
    //     let syncRunLoop : unit Async =
    //         let foo g =
    //             async {
    //                 let (s2, es : _ list, x) = g !state
    //                 state := s2
    //                 for e in es do
    //                     do! sendEvent e
    //                 return x
    //             }
    //         async {
    //             while true do
    //                 f foo
    //         }
    //     Async.Parallel [ syncState; syncRunLoop ] |> Async.Ignore

    // let runRunMain readEvent sendEvent initState readMessage sendMessage =
    //     async {
    //         let f = main readMessage sendMessage
    //         do! runMain readEvent sendEvent initState TimeSpan.Zero f
    //     }

    // let wrapMain (readMessage : Async<UserId * string>) (sendEvent : Events -> _ Async) (sendMessage : UserId * string -> _ Async) state =
    //     ()

    // let init readNewMessage = StoreUpdater.init, [ readNewMessage TextReceived ]

    // let update sendEvent sendMessage (msg : Msg) (state : State) =
    //     match msg with
    //     | EventsReceived e -> StoreUpdater.invoke state e, []
    //     | TextReceived (userId, message) -> Updater2.update sendEvent sendMessage state (userId, message)
    //     | SendMessageEnd | SendEventEnd -> state, []

// let emptyState = StoreUpdater.init
// let restore state e = StoreUpdater.invoke state e

// let executeEffect _ _ _ eff = eff
    // function
    // | Updater.ReadNewMessage f ->
    //     readFromTelegram >>- f
    // | Updater.SendMessage (userId, text) ->
    //     sendToTelegram userId text >>- always Updater.SendMessageEnd
    // | Updater.SendEvent e ->
    //     sendEvent e >>- always Updater.SendEventEnd
