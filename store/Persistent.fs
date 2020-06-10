module Spectator.Store.Persistent

module private Inner =
    open MongoDB.Bson
    open MongoDB.Driver

    let getDatabase mongoDomain database =
        MongoDB.Driver
            .MongoClient(sprintf "mongodb://%s" mongoDomain)
            .GetDatabase(database)

    let insert (db : IMongoDatabase) name item =
        async {
            let collection = db.GetCollection name
            do! item.ToBsonDocument() |> collection.InsertOneAsync |> Async.AwaitTask
        }

    let delete (db : IMongoDatabase) name id = 
        async {
            let collection = db.GetCollection name
            let query = BsonDocument.Create {| id = id |}
            do! collection.DeleteOneAsync(FilterDefinition.op_Implicit query)
                |> Async.AwaitTask |> Async.Ignore 
        }

    let forEach<'a> (db : IMongoDatabase) name f =
        async {
            do! db.GetCollection<'a>(name)
                  .Find(FilterDefinition.op_Implicit "{}")
                  .ForEachAsync(System.Action<_>(fun s -> f s))
                |> Async.AwaitTask
        }

open Spectator.Core

let private database = "spectator"

let restoreState mongoDomain emptyState f =
    async {
        let state = ref emptyState
        let update e = state := f !state e

        let db = Inner.getDatabase mongoDomain database
        do! Inner.forEach db "subscriptions" (fun s -> update <| SubscriptionCreated s)
        do! Inner.forEach db "snapshots" (fun s -> update <| SnapshotCreated s)

        return !state
    }

let main mongoDomain receiceEvent =
    let db = Inner.getDatabase mongoDomain database

    let loop () =
        async {
            match! receiceEvent with
            | SubscriptionCreated sub ->
                do! Inner.insert db "subscriptions" sub
            | SubscriptionRemoved (sids, _) ->
                for id in sids do
                    do! Inner.delete db "subscriptions" id
            | SnapshotCreated snap ->
                do! Inner.insert db "snapshots" snap
            | NewSubscriptionCreated | RestoreFromPersistent -> ()
        }
    loop ()
