module Spectator.Bot.App

open System
open Spectator.Core

module Domain =
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
        | Regex "/rm ([^ ]+)" [ url ] -> DeleteSubscriptionCmd ^ Uri url
        | Regex "/history ([^ ]+)" [ url ] -> History @@ Uri url
        | _ -> UnknownCmd

    // let parse' (message : Bot.Message) = parse message.text

    let snapshotsCount (snapshots : Snapshot list) subId =
        snapshots
        |> List.filter @@ fun sn -> sn.subscriptionId = subId
        |> List.length

    let subListToMessageResponse (subscriptions : Subscription list) newSubscriptions userId snapshots =
        let subs = 
            subscriptions 
            |> List.filter ^ fun x -> x.userId = userId
            |> List.map ^ fun sub -> sprintf "%O '%s' (%i)" sub.uri sub.filter (snapshotsCount snapshots sub.id)
        newSubscriptions
        |> List.filter ^ fun x -> x.userId = userId
        |> List.map ^ fun x -> sprintf "(Waiting) %O '%s'" x.uri x.filter
        |> List.append subs
        |> List.fold (sprintf "%s\n- %s") "Your subscriptions: "

    let deleteSubs (subscriptions : Subscription list) newSubscriptions userId uri =
        newSubscriptions |> List.filter ^ fun x -> x.userId <> userId || x.uri <> uri
        , subscriptions |> List.filter ^ fun x -> x.userId <> userId || x.uri <> uri

    let createNewSub user uri filter =
        { id = TypedId.wrap Guid.Empty; userId = user; uri = uri; filter = Option.defaultValue "" filter }

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

module Updater =
    module P = Domain

    type UserState =
        { subscriptions : Subscription list
          newSubscriptions : NewSubscription list
          snapshots : Snapshot list
          response : string }
    type State = { states : Map<UserId, UserState> }

    let private findUserBySnapshot (state : State) (snap : Snapshot) : UserId option =
        state.states
        |> Map.tryFindKey @@ fun _ v -> 
            v.subscriptions 
            |> List.exists @@ fun sub -> sub.id = snap.subscriptionId

    let private updateUserState (state : State) (userId : UserId) (f : UserState -> UserState * 'a list) : State * 'a list =
        let us =
            Map.tryFind userId state.states
            |> Option.defaultValue { snapshots = []; subscriptions = []; newSubscriptions = []; response = ""}
        let (us, effs) = f us
        { state with states = Map.add userId us state.states }, effs

    let init = { states = Map.empty }

    let private handleBotMessage (user : UserId) text (db : UserState) =
        match P.parse text with
        | P.History url -> 
            { db with response = Domain.getHistory db.snapshots db.subscriptions user url }
            , []
        | P.GetUserSubscriptionsCmd ->
            { db with response = Domain.subListToMessageResponse db.subscriptions db.newSubscriptions user db.snapshots }
            , []
        | P.DeleteSubscriptionCmd uri ->
            let (ns, ss) = Domain.deleteSubs db.subscriptions db.newSubscriptions user uri
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
            { db with 
                subscriptions = ss; newSubscriptions = ns
                response = "Your subscription deleted" }
            , [ SubscriptionRemoved (remSubs, rmNewSubs) ]
        | P.AddNewSubscriptionCmd (uri, filter) ->
            let newSub = Domain.createNewSub user uri filter
            { db with 
                newSubscriptions = newSub :: db.newSubscriptions
                response = "Your subscription created" }
            , [ NewSubscriptionCreated newSub ]
        | P.UnknownCmd -> 
            { db with response = "/ls - Show your subscriptions\n/add [url] - Add new subscription\n/rm [url] - Add new subscription\n/history [url] - show last snapshots for subscriptio with url" }
            , []

    let private handleEvent state = 
        function
        | RestoreFromPersistent (subs, snaps) ->
            let state =
                subs
                |> List.fold 
                    (fun state sub -> 
                        updateUserState state sub.userId @@ fun us -> 
                            { us with subscriptions = sub :: us.subscriptions }, []
                        |> fst) 
                    state
            let state =
                snaps
                |> List.fold
                    (fun state snap ->
                        match findUserBySnapshot state snap with
                        | Some userId ->
                            updateUserState state userId @@ fun us -> 
                                { us with snapshots = snap :: us.snapshots }, []
                        | None -> state, []
                        |> fst)
                    state
            state, []
        | SnapshotCreated snap ->
            match findUserBySnapshot state snap with
            | Some userId ->
                updateUserState state userId @@ fun us -> 
                    { us with snapshots = snap :: us.snapshots }, []
            | None -> state, []
        | SubscriptionCreated sub ->
            updateUserState state sub.userId @@ fun us -> 
                { us with subscriptions = sub :: us.subscriptions }, []
        | NewSubscriptionCreated | SubscriptionRemoved -> state, []

    let view (userId : UserId) (state : State) =
        match Map.tryFind userId state.states with
        | Some x -> x.response
        | None -> ""

    type Msg =
        | EventsReceived of Events
        | TextReceived of UserId * string

    let update (msg : Msg) (state : State) =
        match msg with
        | EventsReceived e ->
            handleEvent state e
        | TextReceived (userId, message) -> 
            updateUserState state userId (handleBotMessage userId message)

let emptyState = Updater.init

let restore state e =
    Updater.update (Updater.EventsReceived e) state |> fst

let main initState sendEvent receiveEvent repl =
    let handleEvents events =
        events
        |> List.map sendEvent
        |> Async.Sequential
        |> Async.Ignore
        |> Async.Start

    let state = ref initState

    let rec loopReadEvents () =
        async {
            let! e = receiveEvent
            let (s2, events) = Updater.update (Updater.EventsReceived e) !state
            state := s2
            handleEvents events
            do! loopReadEvents ()
        }
    let handle (user : UserId, text : string) =
        let (s2, events) = Updater.update (Updater.TextReceived (user, text)) !state
        state := s2
        handleEvents events
        Updater.view user s2 |> async.Return

    [ loopReadEvents ()
      repl handle ]
    |> Async.Parallel |> Async.Ignore
