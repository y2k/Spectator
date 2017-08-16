namespace Spectator.Infrastructure

module Bus = 
    open EasyNetQ
    open Spectator.Core
    
    let respondCommandAsync (bus : IBus) f = 
        bus.RespondAsync<Command, Responses>(fun x -> f x |> Async.StartAsTask) 
        |> ignore

module Async = 
    let zip a1 a2 f = async { let! r1 = a1
                              let! r2 = a2
                              return f (r1, r2) }
    let map f a = async { let! r = a
                          return f r }
    let replaceWith x a = async { let! _ = a
                                  return x }

module MongoDb = 
    open MongoDB.Bson
    open MongoDB.Driver
    
    let insert (x : 'a) (collection : IMongoCollection<'a>) = 
        collection.InsertOneAsync x |> Async.AwaitTask
    let insertMany (x : 'a list) (collection : IMongoCollection<'a>) = 
        collection.InsertManyAsync(x) |> Async.AwaitTask
    
    let findWithoutId (filter : string) (collection : IMongoCollection<'a>) = 
        filter
        |> FilterDefinition.op_Implicit
        |> collection.Find<'a>
        |> fun x -> 
            "{_id: 0}"
            |> ProjectionDefinition<_, _>.op_Implicit
            |> x.Project<'a>
            |> fun x -> x.ToListAsync() |> Async.AwaitTask
        |> Async.map List.ofSeq