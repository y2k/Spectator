module Spectator.Infrastructure.MongoCofx

open System
open Spectator.Core

type IMongoProvider =
    abstract member listen : (CoEffectDb -> unit Async) -> unit Async
    abstract member run : Filter -> (CoEffectDb -> CoEffectDb * 'a) -> 'a Async

module private Inner =
    open MongoDB.Bson
    open MongoDB.Driver

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

    let query<'t> (mdb : IMongoDatabase) colName limit offset filter order : 't list Async =
        mdb.GetCollection(colName)
            .Find(FilterDefinition.op_Implicit (match filter with Some f -> f | None -> "{}"))
            .Sort(SortDefinition.op_Implicit (match order with Some o -> o | None -> "{$natural:1}"))
            .Limit(limit |> Option.toNullable)
            .Skip(offset |> Option.toNullable)
            .ToListAsync() |> Async.AwaitTask
        >>- Seq.toList

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

    let mkDatabase tableName =
        let db = MongoDB.Driver
                  .MongoClient(sprintf "mongodb://%s" DependencyGraph.config.mongoDomain)
                  .GetDatabase("spectator")
        let snapshots = db.GetCollection<Snapshot> tableName
        CreateIndexModel(
            IndexKeysDefinition<Snapshot>.op_Implicit("{ subscriptionId : 1, uri : 1 }"),
            CreateIndexOptions(Unique = Nullable true))
        |> snapshots.Indexes.CreateOne |> ignore
        db          

let private fixIds db =
    let fixId (id : _ TypedId) = 
        if id = TypedId.wrap Guid.Empty then TypedId.wrap ^ Guid.NewGuid() else id
    { db with 
        subscriptions = List.map (fun x -> { x with id = fixId x.id }) db.subscriptions 
        newSubscriptions = List.map (fun x -> { x with id = fixId x.id }) db.newSubscriptions }

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
        |> List.map @@ fun f -> Inner.query<Snapshot> mongo R.SnapshotsDb (Some 10) None (Some f) (Some "{$natural:-1}")
        |> Async.Sequential
        >>- List.concat

let private semaphore = new Threading.SemaphoreSlim(1)

let private runCfx (filter : Filter) mongo (f : CoEffectDb -> (CoEffectDb * 'a)) = 
    async {
        do! semaphore.WaitAsync() |> Async.AwaitTask
        try
            let! subs = Inner.query mongo R.SubscriptionsDb None None None None
            let! newSubs = Inner.query mongo R.NewSubscriptionsDb None None None None
            let! snaps = loadSnapshots mongo subs filter
            let db = { subscriptions = subs; newSubscriptions = newSubs; snapshots = ReadLog snaps }
            let (newDb, eff) = f db |> Pair.map fixIds
            if newDb.newSubscriptions <> db.newSubscriptions then
                do! Inner.deleteFromCol mongo R.NewSubscriptionsDb
                do! newDb.newSubscriptions
                    |> Inner.insert mongo R.NewSubscriptionsDb
            if newDb.subscriptions <> db.subscriptions then
                do! Inner.deleteFromCol mongo R.SubscriptionsDb
                do! newDb.subscriptions
                    |> Inner.insert mongo R.SubscriptionsDb
            let snapshots = match newDb.snapshots with WriteLog xs -> xs | ReadLog _ -> []
            if List.isNotEmpty snapshots then
                do! Inner.insert mongo R.SnapshotsDb snapshots
            return eff
        finally
            semaphore.Release() |> ignore
    }

let private subscribeQuery mdb f = 
    async {
        let! offsetStart = Inner.count mdb R.SnapshotsDb
        let mutable offset = int offsetStart

        while true do
            let! snaps = Inner.query mdb R.SnapshotsDb (Some 100) (Some offset) None None
            if List.isNotEmpty snaps then
                do! runCfx NoneFilter mdb ^ fun db -> db, f { db with snapshots = ReadLog snaps }
                    >>= id
                offset <- offset + (List.length snaps)
            do! Async.Sleep 5_000 
    }

let mkProvider () : IMongoProvider =
    let db = Inner.mkDatabase R.SnapshotsDb
    { new IMongoProvider with
        member __.listen f = subscribeQuery db f
        member __.run filter f = runCfx filter db f }
