module Spectator.Worker.RssSnapshotsWorker

open System
open Spectator.Core

type State =
    { subscriptions: Subscription list
      lastUpdated: Map<Subscription TypedId, DateTime> }
    static member empty =
        { subscriptions = []
          lastUpdated = Map.empty }

type private DownloadComplete =
    | DownloadComplete of Subscription * Result<byte[], exn>
    interface Event

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

let handleEvent (state: State) (e: Event) : Command list =
    match e with
    | :? TimerTicked ->
        state.subscriptions
        |> List.map (fun s -> DownloadHttp(s.uri, (fun r -> DownloadComplete(s, r))))
    | :? DownloadComplete as DownloadComplete (sub, Ok bytes) ->
        let lastUpdated =
            state.lastUpdated
            |> Map.tryFind sub.id
            |> Option.defaultValue DateTime.UnixEpoch

        let isNew = state.lastUpdated |> Map.tryFind sub.id |> Option.isSome

        RssParser.Parser.getNodes (Text.Encoding.UTF8.GetString bytes)
        |> List.filter (fun x -> x.created > lastUpdated)
        |> List.map (fixSnapshotFields sub)
        |> List.sortBy (fun x -> x.created)
        |> List.map (fun snap -> SnapshotCreated(isNew, snap) :> Command)
    | _ -> []
