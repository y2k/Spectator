module Spectator.Bot.App

open System
open Spectator.Core

module Domain =

    type UserState =
        { subscriptions : Subscription list
          newSubscriptions : NewSubscription list
          snapshots : Snapshot list }

    type Cmd =
        | History of Uri
        | GetUserSubscriptionsCmd
        | UnknownCmd
        | AddNewSubscriptionCmd of Uri * string option
        | DeleteSubscriptionCmd of Uri

    let parse (message : string) =
        let isValidUri url = Uri.IsWellFormedUriString(url, UriKind.Absolute)
        match message with
        | Regex "/ls" [] -> GetUserSubscriptionsCmd
        | Regex "/add ([^ ]+) ([^ ]+)" [ url; filter ] when isValidUri url -> AddNewSubscriptionCmd (Uri url, Some filter)
        | Regex "/add ([^ ]+)" [ url ] when isValidUri url -> AddNewSubscriptionCmd (Uri url, None)
        | Regex "/rm ([^ ]+)" [ url ] when isValidUri url -> DeleteSubscriptionCmd @@ Uri url
        | Regex "/history ([^ ]+)" [ url ] when isValidUri url -> History @@ Uri url
        | _ -> UnknownCmd

    let snapshotsCount (snapshots : Snapshot list) subId =
        snapshots
        |> List.filter @@ fun sn -> sn.subscriptionId = subId
        |> List.length

    let subToString snapshots (sub : Subscription) =
        let provider =
            match sub.provider with
            | id when id = Guid "E5D3A9F2-325C-4CEF-BCA9-99D23F9E5AE5" -> "RSS"
            | _ -> "?"
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

    let removeSubsWithIds states ids =
        states
        |> Map.map @@ fun _ state ->
            { state with
                newSubscriptions = 
                    state.newSubscriptions 
                    |> List.filter (fun s -> not <| List.contains s.id ids) }

module Updater =
    open Domain

    type State = { states : Map<UserId, UserState> }

    let private findUserBySnapshot (state : State) (snap : Snapshot) : UserId option =
        state.states
        |> Map.tryFindKey @@ fun _ v -> 
            v.subscriptions 
            |> List.exists @@ fun sub -> sub.id = snap.subscriptionId

    let private updateUserState (state : State) (userId : UserId) (f : UserState -> UserState * 'a list) : State * 'a list =
        let us =
            Map.tryFind userId state.states
            |> Option.defaultValue { snapshots = []; subscriptions = []; newSubscriptions = [] }
        let (us, effs) = f us
        { state with states = Map.add userId us state.states }, effs

    type Msg =
        | EventsReceived of Events
        | TextReceived of UserId * string
        | SendMessageEnd
        | SendEventEnd

    type Cmd =
        | SendEvent of Events
        | ReadNewMessage of (UserId * string -> Msg)
        | SendMessage of UserId * string

    let private handleBotMessage (user : UserId) text (db : UserState) =
        match parse text with
        | History url -> 
            db
            , [ SendMessage (user, getHistory db.snapshots db.subscriptions user url) ]
        | GetUserSubscriptionsCmd ->
            db
            , [ SendMessage (user, subListToMessageResponse db.subscriptions db.newSubscriptions user db.snapshots) ]
        | DeleteSubscriptionCmd uri ->
            let (ns, ss) = deleteSubs db.subscriptions db.newSubscriptions user uri
            let remSubs = 
                db.subscriptions
                |> List.map @@ fun x -> x.id
                |> List.filter @@ fun id -> 
                    ss |> List.forall @@ fun si -> si.id <> id
            let rmNewSubs =
                db.newSubscriptions
                |> List.map @@ fun x -> x.id
                |> List.filter @@ fun id -> 
                    ns |> List.forall @@ fun nsi -> nsi.id <> id
            { db with subscriptions = ss; newSubscriptions = ns }
            , [ SendEvent <| SubscriptionRemoved (remSubs, rmNewSubs)
                SendMessage (user, "Your subscription deleted") ]
        | AddNewSubscriptionCmd (uri, filter) ->
            let newSub = createNewSub user uri filter
            { db with newSubscriptions = newSub :: db.newSubscriptions }
            , [ SendEvent <| NewSubscriptionCreated newSub
                SendMessage (user, "Your subscription created") ]
        | UnknownCmd -> 
            db
            , [ SendMessage (user, "/ls - Show your subscriptions\n/add [url] - Add new subscription\n/rm [url] - Add new subscription\n/history [url] - show last snapshots for subscription with url") ]

    let private handleEvent state = function
        | SnapshotCreated snap ->
            match findUserBySnapshot state snap with
            | Some userId ->
                updateUserState state userId @@ fun us ->
                    let snaps = snap :: us.snapshots
                    { us with snapshots = snaps |> List.take (min 10 snaps.Length)}, []
            | None -> state, []
        | SubscriptionCreated sub ->
            updateUserState state sub.userId @@ fun us -> 
                { us with subscriptions = sub :: us.subscriptions }, []
        | SubscriptionRemoved (_, ids) ->
            { state with states = removeSubsWithIds state.states ids }
            , []
        | NewSubscriptionCreated -> state, []
    
    let init = { states = Map.empty }, [ ReadNewMessage TextReceived ]

    let update (msg : Msg) (state : State) =
        match msg with
        | EventsReceived e ->
            handleEvent state e
        | TextReceived (userId, message) ->
            printfn "Telegram (%s) :: %s" userId message
            let state, cmd = updateUserState state userId (handleBotMessage userId message)
            state
            , ReadNewMessage TextReceived :: cmd
        | SendMessageEnd | SendEventEnd -> state, []

let emptyState = Updater.init |> fst

let restore state e =
    Updater.update (Updater.EventsReceived e) state |> fst

let executeEffect sendToTelegram readFromTelegram sendEvent = function
    | Updater.ReadNewMessage f ->
        readFromTelegram >>- f
    | Updater.SendMessage (userId, text) ->
        sendToTelegram userId text >>- always Updater.SendMessageEnd
    | Updater.SendEvent e ->
        sendEvent e >>- always Updater.SendEventEnd

let main initState sendEvent receiveEvent sendToTelegram readFromTelegram =
    let executeEffect = executeEffect sendToTelegram readFromTelegram sendEvent
    Tea.start initState (snd Updater.init) Updater.update Updater.EventsReceived executeEffect receiveEvent
