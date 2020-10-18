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
    let private snapshotsCount counts subId =
        Map.tryFind subId counts
        |> Option.defaultValue 0

    let subToString counts (sub : Subscription) =
        let provider =
            match sub.provider with
            | id when id = Guid "E5D3A9F2-325C-4CEF-BCA9-99D23F9E5AE5" -> "RSS"
            | id when id = Guid "3B26457E-8AB7-41AD-8DEC-11AF891A3052" -> "Telegram"
            | id when id = Guid "AE4FEE1F-C08D-44B9-B526-4162FF1C328C" -> "HTML"
            | id -> sprintf "Unknown (%s)" (id.ToString().Substring(0, 4))
        sprintf "[%s] %O '%s' (%i)" provider sub.uri sub.filter (snapshotsCount counts sub.id)

    let subListToMessageResponse counts (subscriptions : Subscription list) newSubscriptions userId =
        let subs =
            subscriptions
            |> List.filter ^ fun x -> x.userId = userId
            |> List.map (subToString counts)
        newSubscriptions
        |> List.filter ^ fun x -> x.userId = userId
        |> List.map ^ fun x -> sprintf "[Processing...] %O '%s'" x.uri x.filter
        |> List.append subs
        |> List.fold (sprintf "%s\n- %s")  ""
        |> fun x -> "Your subscriptions:" + x

    let deleteSubs (subscriptions : Subscription list) newSubscriptions userId uri =
        newSubscriptions |> List.filter ^ fun x -> x.userId <> userId || x.uri <> uri
        , subscriptions |> List.filter ^ fun x -> x.userId <> userId || x.uri <> uri

    let createNewSub user uri filter =
        { id = TypedId.wrap <| Guid.NewGuid (); userId = user; uri = uri; filter = Option.defaultValue "" filter }

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
          counts : Map<Subscription TypedId, int>
          newSubscriptions : NewSubscription list
          snapshots : Snapshot list }

    type State = { states : Map<UserId, UserState> }

    let updateUserState (state : State) (userId : UserId) (f : UserState -> UserState * _) : State * _ =
        let us =
            Map.tryFind userId state.states
            |> Option.defaultValue { counts = Map.empty; snapshots = []; subscriptions = []; newSubscriptions = [] }
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

    let private removeSubsWithIds states sIds nsIds =
        states
        |> Map.map @@ fun _ state ->
            { state with
                subscriptions =
                    state.subscriptions
                    |> List.filter (fun s -> not <| List.contains s.id sIds)
                newSubscriptions =
                    state.newSubscriptions
                    |> List.filter (fun s -> not <| List.contains s.id nsIds) }

    let private incrimentCount (counts : Map<Subscription TypedId, int>) (snap : Snapshot) =
        let count = Map.tryFind snap.subscriptionId counts
        Map.add snap.subscriptionId ((count |> Option.defaultValue 0) + 1) counts

    let update state = function
        | SnapshotCreated (_, snap) ->
            match findUserBySnapshot state snap with
            | Some userId ->
                updateUserStateSafe state userId @@ fun us ->
                    let snaps = snap :: us.snapshots
                    { us with
                        snapshots = snaps |> List.take (min 10 snaps.Length)
                        counts = incrimentCount us.counts snap }
            | None -> state
        | SubscriptionCreated sub ->
            updateUserStateSafe state sub.userId @@ fun us ->
                { us with subscriptions = sub :: us.subscriptions }
        | SubscriptionRemoved (sIds, nsIds) ->
            { state with states = removeSubsWithIds state.states sIds nsIds }
        | NewSubscriptionCreated ns ->
            updateUserStateSafe state ns.userId @@ fun us ->
                { us with newSubscriptions = ns :: us.newSubscriptions }
        | HealthCheckRequested _ -> state

module HandleTelegramMessage =
    open Store
    module D = Domain
    module P = Parser

    let invoke (user : UserId) text (db : UserState) =
        match P.parse text with
        | P.History url ->
            (user, D.getHistory db.snapshots db.subscriptions user url)
            , []
        | P.GetUserSubscriptionsCmd ->
            (user, D.subListToMessageResponse db.counts db.subscriptions db.newSubscriptions user)
            , []
        | P.DeleteSubscriptionCmd uri ->
            let (remSubs, rmNewSubs) =
                D.deleteSubs db.subscriptions db.newSubscriptions user uri
                ||> D.removeSubs db.subscriptions db.newSubscriptions
            (user, "Your subscription deleted")
            , [ SubscriptionRemoved (remSubs, rmNewSubs) ]
        | P.AddNewSubscriptionCmd (uri, filter) ->
            let newSub = D.createNewSub user uri filter
            (user, "Your subscription created")
            , [ NewSubscriptionCreated newSub ]
        | P.UnknownCmd ->
            (user, "/ls - Show your subscriptions\n/add [url] - Add new subscription\n/rm [url] - Add new subscription\n/history [url] - show last snapshots for subscription with url")
            , []

module Updater =
    open Store

    let main readMessage sendMessage (update : EffectReducer<_, _>) =
        async {
            let! (user, msg) = readMessage

            let! telegramMsg =
                update.invoke @@ fun db ->
                    let (db, (result, events)) =
                        updateUserState db user (fun db -> db, HandleTelegramMessage.invoke user msg db)
                    db, events, result

            let! _ = telegramMsg ||> sendMessage
            return ()
        }

let emptyState = StoreUpdater.init
let restore = StoreUpdater.update
let main = Updater.main
