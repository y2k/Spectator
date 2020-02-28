module Spectator.Infrastructure

open Spectator.Core

type Log() =
    static member log (message : string,
                       [<System.Runtime.CompilerServices.CallerFilePath;
                         System.Runtime.InteropServices.Optional;
                         System.Runtime.InteropServices.DefaultParameterValue("")>] file : string,
                       [<System.Runtime.CompilerServices.CallerLineNumber;
                         System.Runtime.InteropServices.Optional;
                         System.Runtime.InteropServices.DefaultParameterValue(0)>] line : int) =
        printfn "LOG %s:%i :: %s" file line message

module MongoCofx =
    module Converters =
        open MongoDB.Bson.Serialization
        let string wrap unwrap =
            { new Serializers.SealedClassSerializerBase<_>() with
                override __.Deserialize (ctx, _) = ctx.Reader.ReadString() |> wrap
                override __.Serialize (ctx, _, value) = ctx.Writer.WriteString(unwrap value) }
            |> BsonSerializer.RegisterSerializer

    open System
    open MongoDB.Bson
    open MongoDB.Driver
    module R = Spectator.Core.MongoCollections

    module Effects =
        let insert (db : IMongoDatabase) colName items = async {
            do! items
                |> List.map ^ fun x ->
                    Async.wrapTask ^ fun _ ->
                        (db.GetCollection colName).InsertOneAsync <| x.ToBsonDocument()
                |> Async.seq
                >>- List.iter (sprintf "Error %O" >> Log.log) }

        let query (mdb : IMongoDatabase) colName limit offset =
            mdb.GetCollection(colName)
                .Find(FilterDefinition.op_Implicit "{}")
                .Sort(SortDefinition.op_Implicit "{$natural:-1}")
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

    let private fixId (id : Subscription TypedId) = 
        if id = TypedId.wrap Guid.Empty 
            then TypedId.wrap ^ Guid.NewGuid() 
            else id

    let runCfx mongo (f : CoEffectDb -> (CoEffectDb * 'a)) = async {
        do! semaphore.WaitAsync() |> Async.AwaitTask
        try
            let! subs = Effects.query mongo R.SubscriptionsDb None None
            let! newSubs = Effects.query mongo R.NewSubscriptionsDb None None
            let db = { subscriptions = subs; newSubscriptions = newSubs; snapshots = EventLog [] }
            let (newDb, eff) = f db
            let newDb = 
                { newDb with 
                    subscriptions = List.map (fun x -> { x with id = fixId x.id }) db.subscriptions 
                    newSubscriptions = List.map (fun x -> { x with id = fixId x.id }) db.newSubscriptions }
            if newDb.newSubscriptions <> db.newSubscriptions then
                do! Effects.deleteFromCol mongo R.NewSubscriptionsDb
                do! newDb.newSubscriptions
                    |> Effects.insert mongo R.NewSubscriptionsDb
            if newDb.subscriptions <> db.subscriptions then
                do! Effects.deleteFromCol mongo R.SubscriptionsDb
                do! newDb.subscriptions
                    |> Effects.insert mongo R.SubscriptionsDb
            let (EventLog snapshots) = newDb.snapshots
            if not <| List.isEmpty snapshots then
                do! Effects.insert mongo R.SnapshotsDb snapshots
            return eff
        finally
            semaphore.Release() |> ignore }

    let inline runCfx0 m f = runCfx m (fun db -> f db, ())

    let subscribeQuery (mdb : IMongoDatabase) f = async {
        let col = mdb.GetCollection R.SnapshotsDb
        let! offsetStart = col.CountDocumentsAsync(FilterDefinition.op_Implicit "{}") |> Async.AwaitTask
        let mutable offset = int offsetStart

        while true do
            let! snaps = Effects.query mdb R.SnapshotsDb (Some 100) (Some offset)
            if List.isNotEmpty snaps then
                do! runCfx mdb ^ fun db -> db, f { db with snapshots = EventLog snaps }
                    >>= id
                offset <- offset + (List.length snaps)
            do! Async.Sleep 5_000 }
