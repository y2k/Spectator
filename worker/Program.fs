module Spectator.Worker.App

open Spectator.Core
type L = Spectator.Infrastructure.Log
type IParser = Spectator.Worker.HtmlProvider.IParse

module private Domain =
    open MongoDB.Bson
    open System
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

module private Effects =
    let private apply parsers requets f =
        requets
        |> List.map ^ fun (p, url) ->
            let x : IParser = Map.find p parsers
            f x url
        |> Async.Parallel 
        >>- fun xs -> List.ofArray xs
    let isValid parsers requests = 
        apply parsers requests ^ fun x url -> x.isValid url
    let loadSnapshots parsers requests =
        apply parsers requests ^ fun x url -> x.getNodes url
        >>- List.zip requests

module M = Spectator.Infrastructure.MongoCofx

let start env mdb = async {
    let parsers = Map.ofList [
        Provider.Rss, RssParser.RssParse
        Provider.Telegram, Spectator.Worker.TelegramParser.TelegramConnectorApiImpl :> IParser
        Provider.Html, HtmlProvider.HtmlParse(env) :> IParser ]

    while true do
        L.log "Start syncing..."

        let! subReqs = M.runCfx mdb ^ fun db -> db, Domain.loadNewSubs db
        L.log (sprintf "LOG :: new subscriptions requests %A" subReqs)

        let! subResps = Effects.isValid parsers subReqs
        L.log ^ sprintf "LOG :: new subscriptions results %A" subResps

        do! M.runCfx mdb ^ fun db -> Domain.saveSubs db subReqs subResps, ()

        let! newSnapshots =
            M.runCfx mdb ^ fun db -> db, Domain.loadSnapshots db
            >>= Effects.loadSnapshots parsers
        L.log ^ sprintf "LOG :: new snapshots %A" newSnapshots

        do! M.runCfx mdb ^ fun db ->
                Domain.mkSnapshotSaveCommands db newSnapshots, ()

        L.log "End syncing, waiting..."
        do! Async.Sleep 600_000 }
