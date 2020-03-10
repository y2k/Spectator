module Spectator.Infrastructure

open Spectator.Core

module MongoCofx =
    open System
    open MongoDB.Bson
    open MongoDB.Driver
    module R = Spectator.Core.MongoCollections

    module private Effects =
        let private printErrors (results : Result<_, exn> list) =
            results
            |> List.iter ^
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

    let private semaphore = new Threading.SemaphoreSlim(1)

    let private fixIds db =
        let fixId (id : Subscription TypedId) = 
            if id = TypedId.wrap Guid.Empty 
                then TypedId.wrap ^ Guid.NewGuid() 
                else id
        { db with 
            subscriptions = List.map (fun x -> { x with id = fixId x.id }) db.subscriptions 
            newSubscriptions = List.map (fun x -> { x with id = fixId x.id }) db.newSubscriptions }

    let private loadSnapshots mongo (subs : Subscription list) = function
        | NoneFilter -> async.Return []
        | UserFilter user ->
            subs 
            |> List.choose (fun s -> if s.userId = user then Some s.id else None)
            |> List.map @@ fun id -> sprintf "{subscriptionId: %O}" @@ id.ToJson()
            |> List.map @@ fun f -> Effects.query<Snapshot> mongo R.SnapshotsDb (Some 10) None (Some f) (Some "{$natural:-1}")
            |> Async.Sequential
            >>- List.concat

    let runCfx (filter : Filter) mongo (f : CoEffectDb -> (CoEffectDb * 'a)) = async {
        do! semaphore.WaitAsync() |> Async.AwaitTask
        try
            let! subs = Effects.query mongo R.SubscriptionsDb None None None None
            let! newSubs = Effects.query mongo R.NewSubscriptionsDb None None None None
            let! snaps = loadSnapshots mongo subs filter
            let db = { subscriptions = subs; newSubscriptions = newSubs; snapshots = ReadLog snaps }
            let (newDb, eff) = f db |> Pair.map fixIds
            if newDb.newSubscriptions <> db.newSubscriptions then
                do! Effects.deleteFromCol mongo R.NewSubscriptionsDb
                do! newDb.newSubscriptions
                    |> Effects.insert mongo R.NewSubscriptionsDb
            if newDb.subscriptions <> db.subscriptions then
                do! Effects.deleteFromCol mongo R.SubscriptionsDb
                do! newDb.subscriptions
                    |> Effects.insert mongo R.SubscriptionsDb
            let snapshots = match newDb.snapshots with WriteLog xs -> xs | ReadLog _ -> []
            if List.isNotEmpty snapshots then
                do! Effects.insert mongo R.SnapshotsDb snapshots
            return eff
        finally
            semaphore.Release() |> ignore }

    let subscribeQuery (mdb : IMongoDatabase) f = async {
        let col = mdb.GetCollection R.SnapshotsDb
        let! offsetStart = col.CountDocumentsAsync(FilterDefinition.op_Implicit "{}") |> Async.AwaitTask
        let mutable offset = int offsetStart

        while true do
            let! snaps = Effects.query mdb R.SnapshotsDb (Some 100) (Some offset) None None
            if List.isNotEmpty snaps then
                do! runCfx NoneFilter mdb ^ fun db -> db, f { db with snapshots = ReadLog snaps }
                    >>= id
                offset <- offset + (List.length snaps)
            do! Async.Sleep 5_000 }
