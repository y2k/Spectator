module Spectator.Worker.App

open System
open Spectator.Core
type IParser = HtmlProvider.IParse

module Domain =
    let toSubscription subResps (newSub : NewSubscription) =
        let providerId =
            subResps
            |> List.tryPick ^ fun ((id, uri), suc) ->
                match suc with
                | Ok suc -> if suc && uri = newSub.uri then Some id else None
                | Error _ -> None
        { id = TypedId.wrap Guid.Empty
          userId = newSub.userId
          provider = providerId |> Option.defaultValue Guid.Empty
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
    type Msg = 
        | MkSubscriptions of PluginId list
        | MkSubscriptionsEnd of (Request * Result<bool, exn>) list
        | MkNewSnapshots 
        | MkNewSnapshotsEnd of (Request * Result<Snapshot list, exn>) list
    type 'a Cmd = 
        | LoadSubscriptions of Request list * ((Request * Result<bool, exn>) list -> 'a)
        | LoadSnapshots of Request list * ((Request * Result<Snapshot list, exn>) list -> 'a)

    let update msg (db : CoEffectDb) =
        match msg with
        | MkSubscriptions parserIds ->
            db,
            db.newSubscriptions
            |> List.map ^ fun x -> x.uri
            |> List.allPairs parserIds
            |> Cmd.map LoadSubscriptions MkSubscriptionsEnd
        | MkSubscriptionsEnd subResps ->        
            let subs = db.subscriptions @ List.map (Domain.toSubscription subResps) db.newSubscriptions
            { db with
                subscriptions = List.filter (fun x -> x.provider <> Guid.Empty) subs
                newSubscriptions = Domain.removeSubs db.newSubscriptions subs }, Cmd.none
        | MkNewSnapshots -> 
            db,
            db.subscriptions
            |> List.map ^ fun x -> x.provider, x.uri
            |> Cmd.map LoadSnapshots MkNewSnapshotsEnd
        | MkNewSnapshotsEnd responses -> 
            db.subscriptions
            |> Domain.mkSnapshots responses
            |> fun ss -> { db with snapshots = EventLog ss }, Cmd.none

    module Interpreator =
        let rec run (parsers : HtmlProvider.IParse list) update intMsg =
            let runPlugin requests f g =
                async {
                    let! responses =
                        requests
                        |> List.map ^ fun (pluginId, uri) ->
                            parsers
                            |> List.find ^ fun p -> p.id = pluginId
                            |> fun p -> g p uri
                            |> Async.catch
                            |> Async.map ^ fun r -> 
                                match r with
                                | Error e -> eprintfn "[LOG][ERROR][SYNC] (%O, %O) %O"  pluginId uri e | _ -> ()
                                (pluginId, uri), r
                        |> Async.Sequential
                    let msg = f (responses |> Array.toList)
                    return! run parsers update msg  }
            async {
                let! cmds = DependencyGraph.dbEff.run (update intMsg)
                for cmd in cmds do
                    return!
                        match cmd with
                        | LoadSubscriptions (requests, f) -> runPlugin requests f (fun p uri -> p.isValid uri)
                        | LoadSnapshots (requests, f) -> runPlugin requests f (fun p uri -> p.getNodes uri) }

let start (parsers : HtmlProvider.IParse list) = 
    async {
        let log = Spectator.Infrastructure.Log.log
        let parserIds = parsers |> List.map ^ fun p -> p.id
        while true do
            log "Start syncing..."

            do! Services.Interpreator.run parsers Services.update (Services.MkSubscriptions parserIds)
            do! Services.Interpreator.run parsers Services.update Services.MkNewSnapshots

            log "End syncing, waiting..."
#if DEBUG
            do! Async.Sleep 10_000 
#else
            do! Async.Sleep 600_000 
#endif
    }
