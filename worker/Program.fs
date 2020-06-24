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

    let filterSnapshot (sub : Subscription) snap =
        if String.IsNullOrEmpty sub.filter
            then true
            else Text.RegularExpressions.Regex.IsMatch (snap.title, sub.filter)

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
        |> List.filter (uncurry filterSnapshot)
        |> List.map fixSnapshotFields
        |> List.sortBy @@ fun x -> x.created

    let removeSubs newSubscriptions (subs : Subscription list) =
        List.exceptBy subs (fun (ns : NewSubscription) sub -> ns.uri = sub.uri) newSubscriptions

    let filterNewSnapshots (lastUpdated : Map<Subscription TypedId, DateTime>) snapshots =
        snapshots
        |> List.filter @@ fun snap ->
            match Map.tryFind snap.subscriptionId lastUpdated with
            | Some date -> snap.created > date
            | None -> false

    let udpateLastUpdates (lastUpdated : Map<Subscription TypedId, DateTime>) snapshots =
        snapshots
        |> List.fold 
            (fun xs snap ->
                match Map.tryFind snap.subscriptionId xs with
                | Some date -> Map.add snap.subscriptionId (max date snap.created) xs
                | None -> Map.add snap.subscriptionId snap.created xs)
            lastUpdated

module Services =
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
                { state with
                    subscriptions = 
                        state.subscriptions 
                        |> List.filter (fun s -> not <| List.contains s.id sids) }
                , []
            | SubscriptionCreated sub ->
                { state with subscriptions = sub :: state.subscriptions }, []
            | SnapshotCreated snap -> 
                { state with lastUpdated = Domain.udpateLastUpdates state.lastUpdated [ snap ] }, []
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
            let newSnaps = snapshots |> Domain.filterNewSnapshots state.lastUpdated
            let effects =
                newSnaps
                |> List.map @@ fun sn -> SendEvent (SnapshotCreated sn, always SendEventEnd)
            { state with lastUpdated = Domain.udpateLastUpdates state.lastUpdated snapshots }
            , [ Delay (syncDelay, always MkNewSnapshots) ] @ effects
        | SendEventEnd -> state, []

module Effects =
    let runPlugin f (parsers : HtmlProvider.IParse list) requests =
        requests
        |> List.map @@ fun (pluginId, uri) ->
            parsers
            |> List.find @@ fun p -> p.id = pluginId
            |> fun p -> f p uri
            |> Async.catch
        |> Async.Sequential
        >>- List.ofArray

let emptyState = Services.init |> fst

let restore state e =
    Services.update TimeSpan.Zero [] (Services.EventReceived e) state |> fst

let executeEffect parsers sendEvent = function
    | Services.Delay (time, f) -> 
        Async.Sleep (int time.TotalMilliseconds) >>- f
    | Services.LoadSubscriptions (requests, f) -> 
        Effects.runPlugin (fun p uri -> p.isValid uri) parsers requests >>- f
    | Services.LoadSnapshots (requests, f) -> 
        Effects.runPlugin (fun p uri -> p.getNodes uri) parsers requests >>- f
    | Services.SendEvent (e, f) ->
        sendEvent e >>- f

let main syncDelay initState (parsers : HtmlProvider.IParse list) sendEvent readEvent =
    let update =
        parsers
        |> List.map @@ fun p -> p.id
        |> Services.update syncDelay
    let executeEffect = executeEffect parsers sendEvent
    Tea.start initState (snd Services.init) update Services.EventReceived executeEffect readEvent
