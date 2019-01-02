namespace Spectator.Server.Infrastructure

module Bus = 
    open System
    open EasyNetQ
    open Spectator.Core
    
    let respondCommandAsync (bus : IBus) f = 
        bus.RespondAsync<Command, Responses>(Func<_,_>(f >> Async.StartAsTask))
        |> ignore

module MongoDb = 
    open Spectator.Core
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
        >>- List.ofSeq