module Spectator.Worker.App

module private Domain =
    open System
    open Spectator.Core
    module R = Spectator.Core.MongoCollections

    let mkSnapshotSaveCommands (db : CoEffectDb) (snaps : ((Provider * Uri) * Snapshot list) list) =
        db.subscriptions
        |> List.collect ^ fun sub ->
            snaps |> List.collect ^ fun ((p, u), x) ->
                if p = sub.provider && sub.uri = u then x else []
                |> List.map ^ fun x -> { x with subscriptionId = sub.id }
        |> fun ss -> { db with snapshots = EventLog ss }

    let loadSnapshots (db : CoEffectDb) =
        db.subscriptions
        |> List.map (fun x -> x.provider, x.uri)

    let saveSubs (db : CoEffectDb) requests results =
        let toSubscription (newSub : NewSubscription) =
            let provider =
                requests
                |> List.zip results
                |> List.tryPick ^ fun (suc, (p, uri)) ->
                    if uri = newSub.uri && suc then Some p else None
            { id = System.Guid.NewGuid()
              userId = newSub.userId
              provider = provider |> Option.defaultValue Provider.Invalid
              uri = newSub.uri
              filter = newSub.filter }
        let ws = List.map toSubscription db.newSubscriptions
        { db with newSubscriptions = []; subscriptions = db.subscriptions @ ws }

    let loadNewSubs (db : CoEffectDb) =
        db.newSubscriptions
        |> List.map (fun x -> x.uri)
        |> List.allPairs [ Provider.Rss; Provider.Telegram ]

open Spectator.Core

module private Effects =
    let apply parsers f requests =
        requests
        |> List.map ^ fun (p, url) ->
            let x : Spectator.Worker.HtmlProvider.IParse = Map.find p parsers
            f x url
        |> Async.Parallel 
        >>- List.ofArray
        >>- List.zip requests

type L = Spectator.Infrastructure.Log
module M = Spectator.Infrastructure.MongoCofx

let start parsers mdb = async {
    while true do
        L.log "Start syncing..."

        let! subReqs = M.runCfx mdb ^ fun db -> db, Domain.loadNewSubs db
        L.log (sprintf "LOG :: new subscriptions requests %A" subReqs)

        let! subResps = 
            Effects.apply parsers (fun x url -> x.isValid url) subReqs
            >>- List.map snd
        L.log ^ sprintf "LOG :: new subscriptions results %A" subResps

        do! M.runCfx mdb ^ fun db -> Domain.saveSubs db subReqs subResps, ()

        let! newSnapshots =
            M.runCfx mdb ^ fun db -> db, Domain.loadSnapshots db
            >>= Effects.apply parsers (fun x url -> x.getNodes url)
        L.log ^ sprintf "LOG :: new snapshots %A" newSnapshots

        do! M.runCfx mdb ^ fun db ->
                Domain.mkSnapshotSaveCommands db newSnapshots, ()

        L.log "End syncing, waiting..."
        do! Async.Sleep 600_000 }
