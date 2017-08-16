namespace Spectator.Infrastructure

module MongoDb = 
    open MongoDB.Bson
    open MongoDB.Driver
    
    let findWithoutId (collection : IMongoCollection<'a>) (filter : string) = 
        filter
        |> FilterDefinition.op_Implicit
        |> collection.Find<'a>
        |> fun x -> 
            "{_id: 0}"
            |> ProjectionDefinition<_, _>.op_Implicit
            |> x.Project<'a>
            |> fun x -> x.ToListAsync() |> Async.AwaitTask
    
    let todo() = ()