module Spectator.Worker.App

open Spectator.Core
type L = Spectator.Infrastructure.Log

module private Domain =
    open System
    open MongoDB.Bson
    module R = Spectator.Core.MongoCollections

    let mkSnapshotSaveCommands (db : CoEffectDb) (snaps : ((Provider * Uri) * Snapshot list) list) =
        db.subscriptions
        |> List.collect ^ fun sub ->
            snaps |> List.collect ^ fun ((p, u), x) ->
                if p = sub.provider && sub.uri = u then x else []
                |> List.map ^ fun x -> { x with subscriptionId = sub.id }
        |> List.map ^ fun sn -> R.SnapshotsDb, sn.ToBsonDocument()

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
    open MongoDB.Bson
    open MongoDB.Driver

    let isValid ps =
        ps
        |> List.map (fun (p, url) ->
            match p with
            | Provider.Rss -> RssParser.isValid url
            | Provider.Telegram -> sTelegramApi.isValid url
            | Provider.Html -> HtmlProvider.isValid url
            | _ -> failwithf "%O" p)
        |> Async.Parallel >>- fun xs -> List.ofArray xs

    let loadSnapshots env ps =
        ps
        |> List.map (fun (p, url) ->
            match p with
            | Provider.Rss -> RssParser.getNodes url
            | Provider.Telegram -> sTelegramApi.getNodes url
            | Provider.Html -> HtmlProvider.getNodes env url
            | _ -> failwithf "%O" p)
        |> Async.Parallel
        >>- fun xs -> List.zip ps (List.ofArray xs)

    let saveToDb (db : IMongoDatabase) (commands : (_ * BsonDocument) list) =
        commands
        |> List.map ^ fun (cn, doc) ->
            Async.wrapTask ^ fun _ ->
                let col = db.GetCollection cn
                col.InsertOneAsync doc
        |> Async.seq |> Async.Ignore // TODO: логировать важные ошибки

module private Services =
    open Spectator.Infrastructure

    let init env mongo = async {
        let! subReqs = runCfx mongo ^ fun db -> db, Domain.loadNewSubs db
        L.log (sprintf "LOG :: new subscriptions requests %A" subReqs)

        let! subResps = Effects.isValid subReqs
        L.log ^ sprintf "LOG :: new subscriptions results %A" subResps

        do! runCfx mongo ^ fun db -> Domain.saveSubs db subReqs subResps, ()

        let! newSnapshots =
            runCfx mongo ^ fun db -> db, Domain.loadSnapshots db
            >>= Effects.loadSnapshots env
        L.log ^ sprintf "LOG :: new snapshots %A" newSnapshots

        do! runCfx mongo
                ^ fun db -> db, Domain.mkSnapshotSaveCommands db newSnapshots
            >>= Effects.saveToDb mongo }

let rec start env db = async {
    while true do
        L.log "Start syncing..."
        do! Services.init env db
        L.log "End syncing, waiting..."
        do! Async.Sleep 600_000 }
