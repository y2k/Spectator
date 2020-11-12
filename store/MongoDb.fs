module Spectator.Store.MongoDb

open System
open MongoDB.Bson
open MongoDB.Driver

type t =
    { db: IMongoDatabase
      fake: Map<string, obj []> ref }

module Fake =
    let make () = { db = null; fake = ref Map.empty }

    let insert (db: t) name (item: 'a): unit Async = async.Zero()
    let delete (db: t) name (id: Guid): unit Async = async.Zero()
    let forEach<'a> (db: t) (name: string) (f: 'a -> unit): unit Async = async.Zero()

let make mongoDomain database =
    MongoClient(sprintf "mongodb://%s" mongoDomain).GetDatabase(database)
    |> fun db -> { db = db; fake = ref Map.empty }

let insert (db: t) name (item: 'a) =
    if isNull db.db then
        Fake.insert db name item
    else
        async {
            let collection = db.db.GetCollection name
            do! item
                |> collection.InsertOneAsync
                |> Async.AwaitTask
        }

let delete (db: t) name (id: Guid) =
    if isNull db.db then
        Fake.delete db name id
    else
        async {
            let collection = db.db.GetCollection name
            let query = ({| id = id |}).ToBsonDocument()
            do! collection.DeleteOneAsync(FilterDefinition.op_Implicit query)
                |> Async.AwaitTask
                |> Async.Ignore
        }

let forEach<'a> (db: t) name (f: 'a -> unit) =
    if isNull db.db then
        Fake.forEach<'a> db name f
    else
        async {
            do! db.db.GetCollection<'a>(name).Find(FilterDefinition.op_Implicit "{}")
                    .ForEachAsync(Action<_>(fun s -> f s))
                |> Async.AwaitTask
        }
