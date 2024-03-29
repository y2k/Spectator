module Spectator.Worker.RssSnapshotsWorker

open System
open Spectator.Core
open Y2k.EventBus

type State =
    { subscriptions: Subscription list
      lastUpdated: Map<Subscription TypedId, DateTime> }
    interface Command

    static member empty =
        { subscriptions = []
          lastUpdated = Map.empty }

let handleStateCmd (state: State) (cmd: Command) : State =
    match cmd with
    | :? SubscriptionCreated as (SubscriptionCreated sub) -> { state with subscriptions = sub :: state.subscriptions }
    | :? SubscriptionRemoved as SubscriptionRemoved (sids, _) ->
        { state with subscriptions = state.subscriptions |> List.filter (fun s -> not <| List.contains s.id sids) }
    | :? SnapshotCreated as SnapshotCreated (_, snap) ->
        let lastUpdated =
            state.lastUpdated
            |> Map.tryFind snap.subscriptionId
            |> Option.defaultValue DateTime.UnixEpoch

        { state with lastUpdated = Map.add snap.subscriptionId (max lastUpdated snap.created) state.lastUpdated }
    | _ -> state

let private fixSnapshotFields (sub: Subscription) snap =
    { snap with
        subscriptionId = sub.id
        id = TypedId.wrap <| Guid.NewGuid() }

type DownloadComplete =
    | DownloadComplete of Uri list * Result<byte[], exn> list
    interface Event

let private handleTimerEvent state : Command list =
    state.subscriptions
    |> List.map (fun s -> s.uri)
    |> fun uris -> [ DownloadHttp(uris, (fun r -> DownloadComplete(uris, r))) ]

let private handleDownloadEvent state (DownloadComplete (uris, responses)) =
    let lastUpdated (sub: Subscription) =
        state.lastUpdated
        |> Map.tryFind sub.id
        |> Option.defaultValue DateTime.UnixEpoch

    let isNew (sub: Subscription) =
        state.lastUpdated |> Map.tryFind sub.id |> Option.isSome

    let responses =
        Seq.zip uris responses
        |> Seq.choose (fun (k, v) ->
            match v with
            | Ok d -> Some(string k, d)
            | Error _ -> None)
        |> Map.ofSeq

    state.subscriptions
    |> List.choose (fun sub -> Map.tryFind (string sub.uri) responses |> Option.map (fun data -> sub, data))
    |> List.collect (fun (sub, data) ->
        RssParser.Parser.getNodes (Text.Encoding.UTF8.GetString data)
        |> List.filter (fun x -> x.created > lastUpdated sub)
        |> List.map (fixSnapshotFields sub)
        |> List.sortBy (fun x -> x.created)
        |> List.map (fun snap -> SnapshotCreated(isNew sub, snap) :> Command))
    |> List.append [ InitializeCompleted ]

let handleEvent state (e: Event) : Command list =
    match e with
    | :? Initialize -> handleTimerEvent state
    | :? DownloadComplete as e -> handleDownloadEvent state e
    | _ -> []
