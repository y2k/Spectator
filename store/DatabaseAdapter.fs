module Spectator.Store.DatabaseAdapter

open System
open LiteDB

type t = { db: ILiteDatabase }

let make (database: string) =
    let db = new LiteDatabase(database)
    { db = db }

let insert (db: t) (name: string) (item: BsonDocument) =
    async {
        let collection = db.db.GetCollection name
        collection.Insert(item) |> ignore
    }

let queryAll (db: t) (name: string) f =
    async {
        let col = db.db.GetCollection<BsonDocument>(name)
        let result = col.FindAll()

        result
        |> Seq.iter (fun x -> f x |> Async.RunSynchronously)
    }

let delete (db: t) (name: string) (id: Guid) =
    async {
        let collection = db.db.GetCollection name

        collection.Delete(BsonValue.op_Implicit id)
        |> ignore
    }
