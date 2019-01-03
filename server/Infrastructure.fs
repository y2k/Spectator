namespace Spectator.Server.Infrastructure

module MongoDb =
    open Spectator.Core
    open MongoDB.Driver
    open MongoDB.Bson

    let insert (x : 'a) (collection : IMongoCollection<'a>) = collection.InsertOneAsync x |> Async.AwaitTask
    let insertMany (x : 'a list) (collection : IMongoCollection<'a>) = collection.InsertManyAsync(x) |> Async.AwaitTask

    let findWithoutId'<'a> (db : IMongoDatabase) name =
        async {
            let collection = db.GetCollection(name)
            let c = collection.Find<'a>(FilterDefinition<'a>.Empty)
            return! ProjectionDefinition<_, _>.op_Implicit "{_id: 0}"
                    |> fun d -> c.Project<'a>(d).ToListAsync()
                    |> Async.AwaitTask
                    >>- List.ofSeq
        }

    let findWithoutId (filter : string) (collection : IMongoCollection<'a>) =
        filter
        |> FilterDefinition.op_Implicit
        |> collection.Find<'a>
        |> fun x ->
            "{_id: 0}"
            |> ProjectionDefinition<_, _>.op_Implicit
            |> x.Project<'a>
            |> fun x -> x.ToListAsync() |> Async.AwaitTask
            >>- List.ofSeq
