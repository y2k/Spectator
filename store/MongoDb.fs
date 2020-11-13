module Spectator.Store.MongoDb

open System
open MongoDB.Bson
open MongoDB.Driver

type t = { db: IMongoDatabase }

let make mongoDomain database =
    MongoClient(sprintf "mongodb://%s" mongoDomain)
        .GetDatabase(database)
    |> fun db -> { db = db }

let insert (db: t) name (item: BsonDocument) =
    async {
        let collection = db.db.GetCollection name

        do! item
            |> collection.InsertOneAsync
            |> Async.AwaitTask
    }

let queryAll (db: t) name f =
    async {
        do! db
                .db
                .GetCollection<BsonDocument>(name)
                .Find(FilterDefinition.op_Implicit "{}")
                .ForEachAsync(Action<_>(fun s -> f s |> Async.RunSynchronously))
            |> Async.AwaitTask
    }

let delete (db: t) name (id: Guid) =
    async {
        let collection = db.db.GetCollection name
        let query = ({| id = id |}).ToBsonDocument()

        do! collection.DeleteOneAsync(FilterDefinition.op_Implicit query)
            |> Async.AwaitTask
            |> Async.Ignore
    }
