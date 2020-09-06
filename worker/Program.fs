module Spectator.Worker.App

open System
open Spectator.Core

module Domain =
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

    let removeNewSubs (subscriptions : NewSubscription list) sids =
        subscriptions |> List.filter (fun s -> not <| List.contains s.id sids)

    let filterSnapsthos lastUpdated subs snaps =
        let applyUserFilter snap =
            let sub = List.find (fun (sub : Subscription) -> snap.subscriptionId = sub.id) subs
            if String.IsNullOrEmpty sub.filter
                then true
                else Text.RegularExpressions.Regex.IsMatch (snap.title, sub.filter)
        snaps
        |> List.filter applyUserFilter
        |> List.choose @@ fun snap ->
            match Map.tryFind snap.subscriptionId lastUpdated with
            | Some date ->
                if snap.created > date
                    then Some (true, snap)
                    else None
            | None -> Some (false, snap)

    let updateLastUpdates (lastUpdated : Map<Subscription TypedId, DateTime>) snapshots =
        snapshots
        |> List.fold
            (fun lu snap ->
                match Map.tryFind snap.subscriptionId lu with
                | Some date -> Map.add snap.subscriptionId (max date snap.created) lu
                | None -> Map.add snap.subscriptionId snap.created lu)
            lastUpdated

module StoreDomain =
    type State =
        { subscriptions : Subscription list
          newSubscriptions : NewSubscription list
          lastUpdated : Map<Subscription TypedId, DateTime> }

    let init = { subscriptions = []; newSubscriptions = []; lastUpdated = Map.empty }

    let update state event =
        match event with
        | NewSubscriptionCreated ns ->
            { state with newSubscriptions = ns :: state.newSubscriptions }
        | SubscriptionRemoved (sids, nsids) ->
            { state with
                subscriptions = Domain.removeSubs state.subscriptions sids
                newSubscriptions = Domain.removeNewSubs state.newSubscriptions nsids }
        | SubscriptionCreated sub ->
            { state with subscriptions = sub :: state.subscriptions }
        | SnapshotCreated (_, snap) ->
            { state with lastUpdated = Domain.updateLastUpdates state.lastUpdated [ snap ] }

module Module1 =
    open StoreDomain

    let mkSubscriptionsEnd (state : State) (results : list<(PluginId * Uri) * Result<bool, _>>) : Events list =
        let findPlugin (ns : NewSubscription) : PluginId option =
            results
            |> List.tryPick @@ fun ((p, uri), r) ->
                match r with Ok _ when uri = ns.uri -> Some p | _ -> None
        let mkSubscription (newSub : NewSubscription) (p : PluginId) : Subscription =
            { id = TypedId.wrap <| Guid.NewGuid()
              userId = newSub.userId
              provider = p
              uri = newSub.uri
              filter = newSub.filter }
        state.newSubscriptions
        |> List.collect @@ fun ns ->
            findPlugin ns
            |> Option.map @@ fun pid ->
                [ mkSubscription ns pid |> SubscriptionCreated
                  SubscriptionRemoved ([], [ ns.id ]) ]
            |> Option.defaultValue []

module StateMachine =
    open StoreDomain

    type Request = PluginId * Uri

    let mkSubscription (parserIds : PluginId list) state =
        let foo (ns : NewSubscription) =
            parserIds
            |> List.map @@ fun id -> id, ns.uri
        state.newSubscriptions |> List.map foo |> List.concat

    let mkNewSnapshots state =
        state.subscriptions
        |> List.map @@ fun x -> x.provider, x.uri
        |> List.distinct

    let mkNewSnapshotsEnd responses state =
        let snapshots = responses |> Domain.mkSnapshots state.subscriptions
        let effects = snapshots |> Domain.filterSnapsthos state.lastUpdated state.subscriptions
        { state with lastUpdated = Domain.updateLastUpdates state.lastUpdated snapshots }
        , effects |> List.map SnapshotCreated

let emptyState = StoreDomain.init
let restore state e = StoreDomain.update state e

let main parserIds loadSubscriptions loadSnapshots (update : (_ -> _ * Events list) -> _ Async) =
    async {
        let! reqs =
            update @@ fun db -> db, []
            |> Async.map @@ fun db -> StateMachine.mkSubscription parserIds db

        let! responses =
            reqs
            |> List.map loadSubscriptions
            |> Async.Sequential
            |> Async.map @@ fun x -> Seq.zip reqs x |> Seq.toList

        let! _ = update @@ fun db -> db, Module1.mkSubscriptionsEnd db responses

        let! snapReqs =
            update @@ fun db -> db, []
            |> Async.map @@ fun db -> StateMachine.mkNewSnapshots db

        let! snapResp =
            snapReqs
            |> List.map loadSnapshots
            |> Async.Sequential
            |> Async.map @@ fun x -> Seq.zip snapReqs x |> Seq.toList

        let! _ = update @@ StateMachine.mkNewSnapshotsEnd snapResp

        do! Async.Sleep 5_000
    }
