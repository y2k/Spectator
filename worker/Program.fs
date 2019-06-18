module Spectator.Worker.App

open Spectator.Core

module private Domain =
    open MongoDB.Bson
    module R = Spectator.Core.MongoCollections

    let saveSnapshots (db : CoEffectDb) (snaps : Snapshot list list) =
        db.subscriptions
        |> List.zip snaps
        |> List.collect (fun (sn, s) -> sn |> List.map (fun c -> { c with subscriptionId = s.id }))
        |> List.map (fun x -> R.SnapshotsDb, x.ToBsonDocument())

    let loadSnapshots (db : CoEffectDb) =
        db.subscriptions
        |> List.map (fun x -> x.provider, x.uri)

    let saveSubs (db : CoEffectDb) requests results =
        let toSubscription (newSub : NewSubscription) =
            let provider =
                requests
                |> List.zip results
                |> List.tryPick (fun (suc, (p, uri)) -> if uri = newSub.uri && suc then Some p else None)
            { id = System.Guid.NewGuid()
              userId = newSub.userId
              provider = provider |> Option.defaultValue Provider.Invalid
              uri = newSub.uri }
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
            | Provider.Telegram -> TelegramParser.isValid url
            | _ -> failwithf "%O" p)
        |> Async.Parallel >>- fun xs -> List.ofArray xs

    let loadSnapshots ps =
        ps
        |> List.map (fun (p, url) ->
            match p with
            | Provider.Rss -> RssParser.getNodes url
            | Provider.Telegram -> TelegramParser.getNodes url
            | _ -> failwithf "%O" p)
        |> Async.Parallel >>- List.ofArray

    let saveToDb (db : IMongoDatabase) (ws : (string * BsonDocument) list) =
        ws
        |> List.map (fun (collection, value) ->
                let col = db.GetCollection collection
                col.InsertOneAsync value |> Async.AwaitTask)
        |> Async.Parallel |> Async.Ignore

module private Services =
    open MongoDB.Bson
    open Spectator.Infrastructure

    let init mongo =
        async {
            let! subReqs = dbContext mongo ^ fun db -> db, Domain.loadNewSubs db
            let! subResps = Effects.isValid subReqs

            do! dbContext mongo ^ fun db -> Domain.saveSubs db subReqs subResps, ()
            
            let! newSnapshots = 
                dbContext mongo ^ fun db -> db, Domain.loadSnapshots db
                >>= Effects.loadSnapshots

            do! dbContext mongo 
                    ^ fun db -> db, Domain.saveSnapshots db newSnapshots
                >>= Effects.saveToDb mongo
        }

let start db =
    async {
        while true do
            printfn "Start syncing..."
            do! Services.init db
            printfn "End syncing, waiting..."
            do! Async.Sleep 10_000
    }
