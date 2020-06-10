module Spectator.Worker.App

open System
open Spectator.Core

module Domain =
    let toSubscription subResps (newSub : NewSubscription) =
        subResps
        |> List.tryPick ^ fun ((id, uri), suc) ->
            match suc with
            | Ok suc -> if suc && uri = newSub.uri then Some id else None
            | Error _ -> None
        |> Option.map @@ fun providerId ->
            { id = TypedId.wrap Guid.Empty
              userId = newSub.userId
              provider = providerId
              uri = newSub.uri
              filter = newSub.filter }

    let mkSnapshots responses subs =
        subs
        |> List.collect ^ fun sub ->
            responses
            |> List.collect ^ fun ((p, u), snaps) ->
                match snaps with
                | Ok snaps -> if p = sub.provider && u = sub.uri then snaps else []
                | Error _ -> []
            |> List.map ^ fun x -> { x with subscriptionId = sub.id }

    let removeSubs newSubscriptions (subs : Subscription list) =
        List.exceptBy subs (fun (ns : NewSubscription) sub -> ns.uri = sub.uri) newSubscriptions

module Services =
    type Request = PluginId * Uri
    type State = { subscriptions : Subscription list }
    type Msg = 
        | EventReceived of Events
        | MkSubscriptionsEnd of NewSubscription * (Request * Result<bool, exn>) list
        | MkNewSnapshots 
        | MkNewSnapshotsEnd of (Request * Result<Snapshot list, exn>) list
    type 'a Cmd =
        | Delay of TimeSpan * (unit -> 'a)
        | LoadSubscriptions of Request list * (Result<bool, exn> list -> 'a)
        | LoadSnapshots of Request list * (Result<Snapshot list, exn> list -> 'a)
        | SendEvent of Events

    let init = { subscriptions = []; }, [ Delay (TimeSpan.Zero, always MkNewSnapshots) ]

    let update parserIds msg (state : State) =
        match msg with
        | EventReceived event ->
            match event with
            | RestoreFromPersistent (subs, _) ->
                { state with subscriptions = subs @ state.subscriptions }, []
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
            | SubscriptionCreated | SnapshotCreated -> state, []
        | MkSubscriptionsEnd (ns, subResps) ->
            match Domain.toSubscription subResps ns with
            | None -> state, [ SendEvent @@ SubscriptionRemoved ([], [ ns.id ]) ]
            | Some sub ->
                { state with subscriptions = sub :: state.subscriptions }
                , [ SendEvent @@ SubscriptionCreated sub
                    SendEvent @@ SubscriptionRemoved ([], [ ns.id ]) ]
        | MkNewSnapshots ->
            let effects =
                state.subscriptions
                |> List.map ^ fun x -> x.provider, x.uri
                |> fun req -> [ LoadSnapshots (req, fun resp -> MkNewSnapshotsEnd (List.map2 pair req resp)) ]
            state, effects
        | MkNewSnapshotsEnd responses ->
            let effects =
                state.subscriptions
                |> Domain.mkSnapshots responses
                |> List.map @@ fun sn -> SendEvent (SnapshotCreated sn)
            state
            , [ Delay (TimeSpan.FromMinutes 5., always MkNewSnapshots) ] @ effects

module Effects =
    let runPlugin f (parsers : HtmlProvider.IParse list) requests =
        requests
        |> List.map ^ fun (pluginId, uri) ->
            parsers
            |> List.find ^ fun p -> p.id = pluginId
            |> fun p -> f p uri
            |> Async.catch
        |> Async.Sequential
        >>- List.ofArray

type IParser = HtmlProvider.IParse

let emptyState = Services.init |> fst

let restore state e =
    Services.update [] (Services.EventReceived e) state |> fst

let main initState (parsers : HtmlProvider.IParse list) sendEvent =
    let executeEffect (cmd : Services.Msg Services.Cmd) : Services.Msg list Async =
        match cmd with
        | Services.Delay (time, f) -> 
            Async.Sleep (int time.TotalMilliseconds) >>- (f >> List.singleton)
        | Services.LoadSubscriptions (requests, f) -> 
            Effects.runPlugin (fun p uri -> p.isValid uri) parsers requests >>- (f >> List.singleton)
        | Services.LoadSnapshots (requests, f) -> 
            Effects.runPlugin (fun p uri -> p.getNodes uri) parsers requests >>- (f >> List.singleton)
        | Services.SendEvent e ->
            sendEvent e >>- (always [])

    let state = ref initState

    let parserIds = parsers |> List.map @@ fun p -> p.id
    let update = Services.update parserIds

    let rec loopUpdate (msg : Services.Msg list) : unit Async =
        async {
            let (s2, effs) =
                List.fold 
                    (fun (state, effs) m ->
                        let (s2, effs2) = update m state
                        s2, effs2 @ effs) 
                    (!state, []) msg
            state := s2
            for ef in effs do
                let! msg = executeEffect ef
                do! loopUpdate msg
        }

    snd Services.init
    |> List.map executeEffect
    |> Async.Sequential
    |> Async.map (List.concat)
    >>= loopUpdate
