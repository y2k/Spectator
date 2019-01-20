module Spectator.Worker.App

open System
open Spectator.Core

module R = Spectator.Server.App.Repository
module I = Spectator.Worker.Infrastructure

type BsonDocument = MongoDB.Bson.BsonDocument

type CollectionName = string

type MongoDbFilter = string

type MongoDbEffects<'a> =
    | ReadEffect of (CollectionName * MongoDbFilter) list * (BsonDocument list list -> 'a)
    | ChangeDbEffect of (CollectionName * BsonDocument) list * (CollectionName * MongoDbFilter) list * (unit -> 'a)

type SyncEffects =
    | ProviderIsValidEffect of (Provider * Uri) list * (bool list -> SyncEffects)
    | LoadSnapshotsEff of (Provider * Uri) list * (Snapshot list list -> SyncEffects)
    | MongoDbEffects of MongoDbEffects<SyncEffects>
    | NoneEffect

module Domain =
    open MongoDB.Bson
    open MongoDB.Bson.Serialization

    let saveSnapshots (subs : Subscription list) (snaps : Snapshot list list) =
        subs
        |> List.zip snaps
        |> List.collect (fun (sn, s) -> sn |> List.map (fun c -> { c with subscriptionId = s.id }))
        |> List.map (fun x -> R.SnapshotsDb, x.ToBsonDocument())
        |> fun xs -> ChangeDbEffect(xs, [], always NoneEffect)
        |> MongoDbEffects

    let loadSnapshots (bsonSubs : BsonDocument list list) =
        let subs = bsonSubs.[0] |> List.map BsonSerializer.Deserialize<Subscription>
        subs
        |> List.map (fun x -> x.provider, x.uri)
        |> flip (curry LoadSnapshotsEff) (saveSnapshots subs)

    let syncSnapshots = MongoDbEffects <| ReadEffect([ R.SubscriptionsDb, null ], loadSnapshots)

    let private toSubscription (requests : (Provider * Uri) list) (results : bool list) (newSub : NewSubscription) =
        let provider =
            requests
            |> List.zip results
            |> List.tryPick (fun (suc, (p, uri)) ->
                   if uri = newSub.uri && suc then Some p
                   else None)
        { id = System.Guid.NewGuid() // FIXME:
          userId = newSub.userId
          provider = provider |> Option.defaultValue Provider.Invalid
          uri = newSub.uri }

    let saveSubs newSubs requests results =
        let ws =
            newSubs
            |> List.map (toSubscription requests results)
            |> List.map (fun x -> R.SubscriptionsDb, x.ToBsonDocument())

        let del = newSubs |> List.map (fun x -> R.NewSubscriptionsDb, sprintf "{uri: \"%O\"}" x.uri)
        MongoDbEffects <| ChangeDbEffect(ws, del, always NoneEffect)

    let convertToRssSub (xs : BsonDocument list list) =
        let newSubs = xs.[0] |> List.map BsonSerializer.Deserialize<NewSubscription>

        let requests =
            newSubs
            |> List.map (fun x -> x.uri)
            |> List.allPairs [ Provider.Rss; Provider.Telegram ]
        ProviderIsValidEffect(requests, saveSubs newSubs requests)

    let syncSubscriptions = MongoDbEffects <| ReadEffect([ R.NewSubscriptionsDb, null ], convertToRssSub)

module MongoDBService =
    open MongoDB
    open MongoDB.Driver
    open MongoDB.Bson
    open MongoDB.Bson.Serialization

    let private delete (db : IMongoDatabase) colName (filter : MongoDbFilter) =
        async {
            let col = db.GetCollection colName
            do! col.DeleteManyAsync(FilterDefinition.op_Implicit filter)
                |> Async.AwaitTask
                |> Async.Ignore
        }

    let private readList (x : IFindFluent<BsonDocument, BsonDocument>) =
        x.Project(ProjectionDefinition<_, _>.op_Implicit "{}").ToListAsync()
        |> Async.AwaitTask
        >>- Seq.toList

    let rec executeEffects (db : IMongoDatabase) (eff : MongoDbEffects<'a>) =
        async {
            match eff with
            | ReadEffect(readers, callback) ->
                let! xs = readers
                          |> List.map (fun (collection, f) ->
                                 let col = db.GetCollection<BsonDocument> collection

                                 let filter =
                                     if String.IsNullOrEmpty f then "{}"
                                     else f
                                 col.Find(FilterDefinition.op_Implicit filter) |> readList)
                          |> Async.Parallel
                          >>- List.ofArray
                return callback xs
            | ChangeDbEffect(ws, dels, callback) ->
                do! ws
                    |> List.map (fun (collection, value) ->
                           let col = db.GetCollection collection
                           col.InsertOneAsync value |> Async.AwaitTask)
                    |> Async.Parallel
                    |> Async.Ignore
                do! dels
                    |> List.map (uncurry (delete db))
                    |> Async.Parallel
                    |> Async.Ignore
                return callback()
        }

module Services =
    let rec executeEff db eff =
        async {
            match eff with
            | ProviderIsValidEffect(ps, f) ->
                do! ps
                    |> List.map (fun (p, url) ->
                           match p with
                           | Provider.Rss -> RssParser.isValid url
                           | Provider.Telegram -> TelegramParser.isValid url
                           | _ -> failwithf "%O" p)
                    |> Async.Parallel
                    >>= (List.ofArray
                         >> f
                         >> executeEff db)
            | LoadSnapshotsEff(ps, f) ->
                do! ps
                    |> List.map (fun (p, url) ->
                           match p with
                           | Provider.Rss -> RssParser.getNodes url
                           | Provider.Telegram -> TelegramParser.getNodes url
                           | _ -> failwithf "%O" p)
                    |> Async.Parallel
                    >>= (List.ofArray
                         >> f
                         >> executeEff db)
            | MongoDbEffects dbEff -> do! MongoDBService.executeEffects db dbEff >>= executeEff db
            | NoneEffect -> ()
        }

let start db =
    printfn "Start synchronizer..."
    Services.executeEff db Domain.syncSubscriptions *> Services.executeEff db Domain.syncSnapshots
    |> I.executeInLoop 10000

[<EntryPoint>]
let main _ = failwith "TODO"
