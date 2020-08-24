module Spectator.Worker.App

open System
open Spectator.Core

module Domain =
    let toSubscription subResps (newSub : NewSubscription) =
        subResps
        |> List.tryPick @@ fun ((id, uri), suc) ->
            match suc with
            | Ok suc -> if suc && uri = newSub.uri then Some id else None
            | Error _ -> None
        |> Option.map @@ fun providerId ->
            { id = TypedId.wrap <| Guid.NewGuid()
              userId = newSub.userId
              provider = providerId
              uri = newSub.uri
              filter = newSub.filter }

    let mkSnapshots subs responses =
        let fixSnapshotFields ((sub : Subscription), snap) =
            { snap with
                created = snap.created.ToUniversalTime()
                subscriptionId = sub.id
                id = TypedId.wrap <| Guid.NewGuid () }
        let tryFindSub (p, u) =
            subs |> List.filter @@ (fun sub -> p = sub.provider && u = sub.uri)
        responses
        |> List.choose @@ fun (id, result) ->
            result
            |> Result.toOption
            |> Option.map (fun snaps -> id, snaps)
        |> List.collect @@ fun (id, snaps) ->
            tryFindSub id |> List.map (fun sub -> sub, snaps)
        |> List.collect @@ fun (sub, snaps) ->
            snaps |> List.map (fun snap -> sub, snap)
        |> List.map fixSnapshotFields
        |> List.sortBy @@ fun x -> x.created

    let removeSubs (subscriptions : Subscription list) sids =
        subscriptions |> List.filter (fun s -> not <| List.contains s.id sids)

    let filterSnapsthos lastUpdated subs snaps =
        let filterSnapshot snap =
            let sub = List.find (fun (sub : Subscription) -> snap.subscriptionId = sub.id) subs
            if String.IsNullOrEmpty sub.filter
                then true
                else Text.RegularExpressions.Regex.IsMatch (snap.title, sub.filter)
        snaps
        |> List.filter filterSnapshot
        |> List.filter @@ fun snap ->
            match Map.tryFind snap.subscriptionId lastUpdated with
            | Some date -> snap.created > date
            | None -> false


    let updateLastUpdates (lastUpdated : Map<Subscription TypedId, DateTime>) snapshots =
        snapshots
        |> List.fold
            (fun lu snap ->
                match Map.tryFind snap.subscriptionId lu with
                | Some date -> Map.add snap.subscriptionId (max date snap.created) lu
                | None -> Map.add snap.subscriptionId snap.created lu)
            lastUpdated

module StateMachine =
    type Request = PluginId * Uri
    type State = { subscriptions : Subscription list; lastUpdated : Map<Subscription TypedId, DateTime> }
    type Msg =
        | EventReceived of Events
        | MkSubscriptionsEnd of NewSubscription * (Request * Result<bool, exn>) list
        | MkNewSnapshots
        | MkNewSnapshotsEnd of (Request * Result<Snapshot list, exn>) list
        | SendEventEnd
    type 'a Effect =
        | Delay of TimeSpan * (unit -> 'a)
        | LoadSubscriptions of Request list * (Result<bool, exn> list -> 'a)
        | LoadSnapshots of Request list * (Result<Snapshot list, exn> list -> 'a)
        | SendEvent of Events * (unit -> 'a)

    let init = { subscriptions = []; lastUpdated = Map.empty }, [ Delay (TimeSpan.Zero, always MkNewSnapshots) ]

    let update syncDelay parserIds msg (state : State) =
        match msg with
        | EventReceived event ->
            match event with
            | NewSubscriptionCreated ns ->
                let req =
                    parserIds
                    |> List.map @@ fun id -> id, ns.uri
                state, [ LoadSubscriptions (req, fun resp -> MkSubscriptionsEnd (ns, List.map2 pair req resp)) ]
            | SubscriptionRemoved (sids, _) ->
                { state with subscriptions = Domain.removeSubs state.subscriptions sids }
                , []
            | SubscriptionCreated sub ->
                { state with subscriptions = sub :: state.subscriptions }, []
            | SnapshotCreated snap ->
                { state with lastUpdated = Domain.updateLastUpdates state.lastUpdated [ snap ] }, []
        | MkSubscriptionsEnd (ns, subResps) ->
            match Domain.toSubscription subResps ns with
            | None -> state, [ SendEvent (SubscriptionRemoved ([], [ ns.id ]), always SendEventEnd) ]
            | Some sub ->
                state
                , [ SendEvent (SubscriptionCreated sub, always SendEventEnd)
                    SendEvent (SubscriptionRemoved ([], [ ns.id ]), always SendEventEnd) ]
        | MkNewSnapshots ->
            let effects =
                state.subscriptions
                |> List.map @@ fun x -> x.provider, x.uri
                |> List.distinct
                |> fun req -> [ LoadSnapshots (req, fun resp -> MkNewSnapshotsEnd (List.map2 pair req resp)) ]
            state, effects
        | MkNewSnapshotsEnd responses ->
            let snapshots = responses |> Domain.mkSnapshots state.subscriptions
            let newSnaps = snapshots |> Domain.filterSnapsthos state.lastUpdated state.subscriptions
            let effects =
                newSnaps
                |> List.map @@ fun sn -> SendEvent (SnapshotCreated sn, always SendEventEnd)
            { state with lastUpdated = Domain.updateLastUpdates state.lastUpdated snapshots }
            , [ Delay (syncDelay, always MkNewSnapshots) ] @ effects
        | SendEventEnd -> state, []

let emptyState = StateMachine.init |> fst

let restore state e =
    StateMachine.update TimeSpan.Zero [] (StateMachine.EventReceived e) state |> fst

let executeEffect (parsers : {| id : Guid; isValid : Uri -> bool Async; getNodes : Uri -> Snapshot list Async |} list) sendEvent eff =
    let runPlugin f requests =
        requests
        |> List.map @@ fun (pluginId, uri) ->
            parsers
            |> List.find @@ fun p -> p.id = pluginId
            |> fun p -> f p uri
            |> Async.catch
        |> Async.Sequential
        >>- List.ofArray
    match eff with
    | StateMachine.Delay (time, f) ->
        Async.Sleep (int time.TotalMilliseconds) >>- f
    | StateMachine.LoadSubscriptions (requests, f) ->
        runPlugin (fun p uri -> p.isValid uri) requests >>- f
    | StateMachine.LoadSnapshots (requests, f) ->
        runPlugin (fun p uri -> p.getNodes uri) requests >>- f
    | StateMachine.SendEvent (e, f) ->
        sendEvent e >>- f
