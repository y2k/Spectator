module Spectator.Infrastructure.MongoCofx

open System
open Spectator.Core

module private Shared =
    open MongoDB.Bson
    open MongoDB.Driver

    let mkDatabase mongoDomain database uniqueIndexes =
        let db = MongoDB.Driver
                  .MongoClient(sprintf "mongodb://%s" mongoDomain)
                  .GetDatabase(database)
        uniqueIndexes
        |> Map.iter @@ fun tableName (index : string) -> 
            let collection = db.GetCollection tableName
            CreateIndexModel(
                IndexKeysDefinition.op_Implicit(index),
                CreateIndexOptions(Unique = Nullable true))
            |> collection.Indexes.CreateOne |> ignore
        db          

    let private printErrors (results : Result<_, exn> list) =
        results
        |> List.iter @@
            function
            | Ok _ -> ()
            | Error (:? AggregateException as ae) -> 
                match ae.InnerException with
                | :? MongoWriteException as me when me.WriteError.Code = 11000 -> ()
                | e -> Log.elog (string e)
            | Error e -> Log.elog (string e)

    let insert (db : IMongoDatabase) colName items =
        items
        |> List.map ^ fun x ->
            Async.wrapTask ^ fun _ ->
                (db.GetCollection colName).InsertOneAsync <| x.ToBsonDocument()
        |> Async.seq
        >>- printErrors

    let deleteFromCol (db : IMongoDatabase) col = async {
        return!
            db.GetCollection col
            |> fun x -> x.DeleteManyAsync(FilterDefinition.op_Implicit "{}")
            |> Async.AwaitTask |> Async.Ignore }

    let count (db : IMongoDatabase) collectionName =
        async {
            let col = db.GetCollection collectionName
            return! col.CountDocumentsAsync(FilterDefinition.op_Implicit "{}")
        }

    let query<'t> (mdb : IMongoDatabase) colName limit offset filter order : 't list Async =
        mdb.GetCollection(colName)
            .Find(FilterDefinition.op_Implicit (match filter with Some f -> f | None -> "{}"))
            .Sort(SortDefinition.op_Implicit (match order with Some o -> o | None -> "{$natural:1}"))
            .Limit(limit |> Option.toNullable)
            .Skip(offset |> Option.toNullable)
            .ToListAsync() |> Async.AwaitTask
        >>- Seq.toList

    let listenUpdates mdb collection notifyListener =
        async {
            let! initOffset = count mdb collection
            let offset = ref (int initOffset)

            while true do
                let! snaps = query mdb collection (Some 100) (Some !offset) None None

                if List.isNotEmpty snaps then
                    do! notifyListener snaps

                offset := !offset + (List.length snaps)
                do! Async.Sleep 5_000 
        }

// let private fixIds db =
//     let fixId (id : _ TypedId) = 
//         if id = TypedId.wrap Guid.Empty then TypedId.wrap ^ Guid.NewGuid() else id
//     { db with 
//         subscriptions = List.map (fun x -> { x with id = fixId x.id }) db.subscriptions 
//         newSubscriptions = List.map (fun x -> { x with id = fixId x.id }) db.newSubscriptions }

module private R =
    let SnapshotsDb = "snapshots"
    let SubscriptionsDb = "subscriptions"
    let NewSubscriptionsDb = "newSubscriptions"

open MongoDB.Bson

let private loadSnapshots mongo (subs : Subscription list) = function
    | NoneFilter -> async.Return []
    | UserFilter user ->
        subs 
        |> List.choose @@ fun s -> if s.userId = user then Some s.id else None
        |> List.map @@ fun id -> sprintf "{subscriptionId: %O}" @@ id.ToJson()
        |> List.map @@ fun f -> Shared.query<Snapshot> mongo R.SnapshotsDb (Some 10) None (Some f) (Some "{$natural:-1}")
        |> Async.Sequential
        >>- List.concat

let private semaphore = new Threading.SemaphoreSlim(1)

// let private runCfx (filter : Filter) mongo (f : CoEffectDb -> (CoEffectDb * 'a)) =
//     async {
//         do! semaphore.WaitAsync() |> Async.AwaitTask
//         try
//             let! subs = Shared.query mongo R.SubscriptionsDb None None None None
//             let! newSubs = Shared.query mongo R.NewSubscriptionsDb None None None None
//             let! snaps = loadSnapshots mongo subs filter
//             let db = { subscriptions = subs; newSubscriptions = newSubs; snapshots = ReadLog snaps }
            
//             let (newDb, eff) = f db |> Pair.map fixIds

//             if newDb.newSubscriptions <> db.newSubscriptions then
//                 do! Shared.deleteFromCol mongo R.NewSubscriptionsDb
//                 do! newDb.newSubscriptions
//                     |> Shared.insert mongo R.NewSubscriptionsDb
//             if newDb.subscriptions <> db.subscriptions then
//                 do! Shared.deleteFromCol mongo R.SubscriptionsDb
//                 do! newDb.subscriptions
//                     |> Shared.insert mongo R.SubscriptionsDb
//             let snapshots = match newDb.snapshots with WriteLog xs -> xs | ReadLog _ -> []
//             if List.isNotEmpty snapshots then
//                 do! Shared.insert mongo R.SnapshotsDb snapshots

//             return eff
//         finally
//             semaphore.Release() |> ignore
//     }

// let private listenUpdates mdb collection listener =
//     Shared.listenUpdates mdb collection @@ fun snaps ->
//         async {
//             let! effect =
//                 runCfx NoneFilter mdb ^ fun db -> 
//                     db, listener { db with snapshots = ReadLog snaps }
//             return! effect
//         }

// let mkProvider () : IMongoProvider =
//     let db =
//         Map.ofList [ R.SnapshotsDb, "{ subscriptionId : 1, uri : 1 }" ]
//         |> Shared.mkDatabase DependencyGraph.config.mongoDomain "spectator"
//     { new IMongoProvider with
//         member __.listen f = listenUpdates db R.SnapshotsDb f
//         member __.run filter f = runCfx filter db f }
