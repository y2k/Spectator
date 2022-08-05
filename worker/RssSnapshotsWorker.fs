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
    | DownloadComplete of Subscription * Result<byte [], exn>
    interface Event

let handleStateCmd (state: State) (cmd: Command) : State =
    match cmd with
    | :? SubscriptionCreated as (SubscriptionCreated sub) -> { state with subscriptions = sub :: state.subscriptions }
    | :? SubscriptionRemoved as SubscriptionRemoved (sids, _) ->
        { state with
            subscriptions =
                state.subscriptions
                |> List.filter (fun s -> not <| List.contains s.id sids) }
    | :? SnapshotCreated as SnapshotCreated (_, snap) ->
        let lastUpdated =
            state.lastUpdated
            |> Map.tryFind snap.subscriptionId
            |> Option.defaultValue DateTime.UnixEpoch

        { state with lastUpdated = Map.add snap.subscriptionId (max lastUpdated snap.created) state.lastUpdated }
    | _ -> state

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

        RssParser.Parser.getNodes (Text.Encoding.UTF8.GetString bytes)
        |> List.filter (fun x -> x.created > lastUpdated)
        |> List.map (fun snap -> SnapshotCreated(true, snap) :> Command)
    | _ -> []

// module Domain =
//     let mkSnapshots subs responses =
//         let fixSnapshotFields ((sub: Subscription), snap) =
//             { snap with
//                   created = snap.created.ToUniversalTime()
//                   subscriptionId = sub.id
//                   id = TypedId.wrap <| Guid.NewGuid() }

//         let tryFindSub (p, u) =
//             subs
//             |> List.filter
//                @@ (fun sub -> p = sub.provider && u = sub.uri)

//         responses
//         |> List.choose
//            @@ fun (id, result) ->
//                result
//                |> Result.toOption
//                |> Option.map (fun snaps -> id, snaps)
//         |> List.collect
//            @@ fun (id, snaps) -> tryFindSub id |> List.map (fun sub -> sub, snaps)
//         |> List.collect
//            @@ fun (sub, snaps) -> snaps |> List.map (fun snap -> sub, snap)
//         |> List.map fixSnapshotFields
//         |> List.sortBy @@ fun x -> x.created

//     let removeSubs (subscriptions: Subscription list) sids =
//         subscriptions
//         |> List.filter (fun s -> not <| List.contains s.id sids)

//     let filterSnapsthos lastUpdated subs snaps =
//         let applyUserFilter snap =
//             let sub =
//                 List.find (fun (sub: Subscription) -> snap.subscriptionId = sub.id) subs

//             if String.IsNullOrEmpty sub.filter then
//                 true
//             else
//                 Text.RegularExpressions.Regex.IsMatch(snap.title, sub.filter)

//         snaps
//         |> List.filter applyUserFilter
//         |> List.choose
//            @@ fun snap ->
//                match Map.tryFind snap.subscriptionId lastUpdated with
//                | Some date ->
//                    if snap.created > date then
//                        Some(true, snap)
//                    else
//                        None
//                | None -> Some(false, snap)

//     let updateLastUpdates (lastUpdated: Map<Subscription TypedId, DateTime>) snapshots =
//         snapshots
//         |> List.fold
//             (fun lu snap ->
//                 match Map.tryFind snap.subscriptionId lu with
//                 | Some date -> Map.add snap.subscriptionId (max date snap.created) lu
//                 | None -> Map.add snap.subscriptionId snap.created lu)
//             lastUpdated

// module StoreDomain =
//     let update state (event: Event) =
//         match event with
//         | :? SubscriptionCreated as (SubscriptionCreated sub) ->
//             { state with
//                   subscriptions = sub :: state.subscriptions }
//         | :? SnapshotCreated as SnapshotCreated (_, snap) ->
//             { state with
//                   lastUpdated = Domain.updateLastUpdates state.lastUpdated [ snap ] }
//         | :? SubscriptionRemoved as SubscriptionRemoved (sids, nsids) ->
//             { state with
//                   subscriptions = Domain.removeSubs state.subscriptions sids }
//         |  _-> state

//     let mkNewSnapshots state =
//         state.subscriptions
//         |> List.map @@ fun x -> x.provider, x.uri
//         |> List.distinct

//     let mkNewSnapshotsEnd responses state : _ * _ list * _ =
//         let snapshots =
//             responses
//             |> Domain.mkSnapshots state.subscriptions

//         let effects =
//             snapshots
//             |> Domain.filterSnapsthos state.lastUpdated state.subscriptions

//         { state with
//               lastUpdated = Domain.updateLastUpdates state.lastUpdated snapshots },
//         effects |> List.map (fun x ->SnapshotCreated x :> Event),
//         ()

// let restore = StoreDomain.update

// let main loadSnapshots (reduce: IReducer<State, Event>) =
//     async {
//         let! snapReqs = reduce.Invoke(fun db -> db, [], StoreDomain.mkNewSnapshots db)

//         let! snapResp =
//             snapReqs
//             |> List.map loadSnapshots
//             |> Async.Sequential
//             |> Async.map (fun x -> Seq.zip snapReqs x |> Seq.toList)

//         do! reduce.Invoke(fun db -> StoreDomain.mkNewSnapshotsEnd snapResp db)
//     }
