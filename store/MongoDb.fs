module Spectator.Store.MongoDb

open MongoDB.Bson
open MongoDB.Driver

type t = { db : IMongoDatabase }

let getDatabase mongoDomain database =
    MongoDB.Driver
        .MongoClient(sprintf "mongodb://%s" mongoDomain)
        .GetDatabase(database)
    |> fun db -> { db = db }

let insert (db : t) name (item : 'a) =
    async {
        let collection = db.db.GetCollection name
        do! item |> collection.InsertOneAsync |> Async.AwaitTask
    }

let delete (db : t) name (id : System.Guid) = 
    async {
        let collection = db.db.GetCollection name
        let query = ({| id = id |}).ToBsonDocument()
        // let query = BsonDocument.Create { id = id }
        do! collection.DeleteOneAsync(FilterDefinition.op_Implicit query)
            |> Async.AwaitTask |> Async.Ignore 
    }

let forEach<'a> (db : t) name f =
    async {
        do! db.db.GetCollection<'a>(name)
              .Find(FilterDefinition.op_Implicit "{}")
              .ForEachAsync(System.Action<_>(fun s -> f s))
            |> Async.AwaitTask
    }
