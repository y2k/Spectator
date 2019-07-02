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

module private Effects =
    open MongoDB.Bson
    open MongoDB.Driver

    let saveToDb (db : IMongoDatabase) (ws : (string * BsonDocument) list) =
        ws
        |> List.map ^ fun (collection, value) ->
                let col = db.GetCollection collection
                col.InsertOneAsync value |> Async.AwaitTask
        |> Async.seq |> Async.Ignore

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

module R = Spectator.Core.MongoCollections
open MongoDB.Bson

let private semaphore = new System.Threading.SemaphoreSlim(1)

let runCfx mongo (f : CoEffectDb -> (CoEffectDb * 'a)) = async {
    do! semaphore.WaitAsync() |> Async.AwaitTask
    try
        let! subs = Effects.loadFromMongo mongo R.SubscriptionsDb
        let! newSubs = Effects.loadFromMongo mongo R.NewSubscriptionsDb
        let db = { subscriptions = subs; newSubscriptions = newSubs }
        let (newDb, eff) = f db
        if newDb <> db then
            do! Effects.deleteFromCol mongo R.NewSubscriptionsDb
            do! newDb.newSubscriptions
                |> List.map ^ fun x -> R.NewSubscriptionsDb, x.ToBsonDocument()
                |> Effects.saveToDb mongo
            do! Effects.deleteFromCol mongo R.SubscriptionsDb
            do! newDb.subscriptions
                |> List.map ^ fun x -> R.SubscriptionsDb, x.ToBsonDocument()
                |> Effects.saveToDb mongo
        return eff
    finally
        semaphore.Release() |> ignore }
