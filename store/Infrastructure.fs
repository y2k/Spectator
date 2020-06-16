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
