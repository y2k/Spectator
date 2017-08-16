namespace Spectator.Infrastructure

module Async = 
    let zip a1 a2 f = async { let! r1 = a1
                              let! r2 = a2
                              return f (r1, r2) }
    let map f a = async { let! r = a
                          return f r }

module DB
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
        |> Async.map List.ofSeq
    
    let todo() = ()