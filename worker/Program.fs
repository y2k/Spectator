module Spectator.Worker.App

open Spectator.Core
open System
module R = Spectator.Core.MongoCollections

type CoEffectDb = {
    subscriptions : Subscription list
    newSubscriptions : NewSubscription list
 }

module private Domain =
    open MongoDB.Bson

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
        |> Async.Parallel >>- fun xs -> ps, List.ofArray xs

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

    let loadFromMongo (db : IMongoDatabase) collection =
        let col = db.GetCollection<BsonDocument> collection
        col.Find(FilterDefinition.op_Implicit "{}")
        |> fun x -> x.Project(ProjectionDefinition<_, _>.op_Implicit "{}").ToListAsync()
        |> Async.AwaitTask
        >>- Seq.toList

    let deleteFromCol (db : IMongoDatabase) col =
        async {
            return!
                db.GetCollection col
                |> fun x -> x.DeleteManyAsync(FilterDefinition.op_Implicit "{}")
                |> Async.AwaitTask |> Async.Ignore
        }

module private Services =
    open MongoDB.Bson
    let dbContext mongo (f : CoEffectDb -> CoEffectDb Async) : unit Async =
        async {
            let! subs = Effects.loadFromMongo mongo R.SubscriptionsDb
            let! newSubs = Effects.loadFromMongo mongo R.NewSubscriptionsDb
            let db = { subscriptions = subs; newSubscriptions = newSubs }
            let! newDb = f db
            if newDb <> db then
                do! Effects.deleteFromCol mongo R.NewSubscriptionsDb
                do! newDb.newSubscriptions
                    |> List.map ^ fun x -> R.NewSubscriptionsDb, x.ToBsonDocument()
                    |> Effects.saveToDb mongo
                do! Effects.deleteFromCol mongo R.SubscriptionsDb
                do! newDb.subscriptions
                    |> List.map ^ fun x -> R.SubscriptionsDb, x.ToBsonDocument()
                    |> Effects.saveToDb mongo
        }

    let init mongo =
        (dbContext mongo ^ fun db ->
            Domain.loadNewSubs db
            |> Effects.isValid
            >>- uncurry (Domain.saveSubs db))
        *>
        (dbContext mongo ^ fun db ->
            Domain.loadSnapshots db
            |> Effects.loadSnapshots
            >>- Domain.saveSnapshots db
            >>= Effects.saveToDb mongo
            >>- fun _ -> db)

let start db =
    async {
        while true do
            printfn "Start syncing..."
            do! Services.init db
            printfn "End syncing, waiting..."
            do! Async.Sleep 10_000
    }
