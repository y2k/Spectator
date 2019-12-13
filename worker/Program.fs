module Spectator.Worker.App

module private Domain =
    open Spectator.Core
    module R = Spectator.Core.MongoCollections

    let saveSnapshots (db : CoEffectDb) snaps =
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
            { id = System.Guid.Empty
              userId = newSub.userId
              provider = provider |> Option.defaultValue PluginId.Empty
              uri = newSub.uri
              filter = newSub.filter }
        let ws = List.map toSubscription db.newSubscriptions
        { db with newSubscriptions = []; subscriptions = db.subscriptions @ ws }

    let loadNewSubs (db : CoEffectDb) parserIds =
        db.newSubscriptions
        |> List.map (fun x -> x.uri)
        |> List.allPairs parserIds

open System

open Spectator.Core
type IParse = Spectator.Worker.HtmlProvider.IParse

module private Effects =
    let apply parsers f requests =
        requests
        |> List.map ^ fun (p, url) ->
            let x : IParse = parsers |> List.find ^ fun x -> x.id = p 
            f x url
        |> Async.Parallel 
        >>- List.ofArray
        >>- List.zip requests

type L = Spectator.Infrastructure.Log
module M = Spectator.Infrastructure.MongoCofx

let start (parsers : HtmlProvider.IParse list) mdb = async {
    while true do
        L.log "Start syncing..."

        let parserIds = parsers |> List.map ^ fun p -> p.id
        let! subReqs = M.runCfx mdb ^ fun db -> db, Domain.loadNewSubs db parserIds
        L.log (sprintf "LOG :: new subscriptions requests %A" subReqs)

        let! subResps = 
            Effects.apply parsers (fun x url -> x.isValid url) subReqs
            >>- List.map snd
        L.log ^ sprintf "LOG :: new subscriptions results %A" subResps

        do! M.runCfx0 mdb ^ fun db ->
            Domain.saveSubs db subReqs subResps
            |> fun db -> { db with subscriptions = db.subscriptions |> List.map ^ fun x -> { x with id = Guid.NewGuid() } }

        let! newSnapshots =
            M.runCfx mdb ^ fun db -> db, Domain.loadSnapshots db
            >>= Effects.apply parsers (fun x url -> x.getNodes url)
        L.log ^ sprintf "LOG :: new snapshots %A" newSnapshots

        do! M.runCfx mdb ^ fun db ->
                Domain.saveSnapshots db newSnapshots, ()

        L.log "End syncing, waiting..."
        do! Async.Sleep 600_000 }
