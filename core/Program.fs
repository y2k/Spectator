open System
open System.Threading.Tasks
open EasyNetQ
open MongoDB.Bson
open MongoDB.Driver
open Spectator.Core

let handle (cmd: Command) = 
    async {
         let db = MongoClient("mongodb://localhost").GetDatabase("spectator")
         match cmd with 
         | GetUserSubscriptions userId -> 
             let subs = db.GetCollection<Subscription>("subscriptions")
             let newSubs = db.GetCollection<NewSubscription>("newSubscriptions")

             let! mySubs = userId |> sprintf "{userId: \"%O\"}" 
                                  |> FilterDefinition.op_Implicit 
                                  |> subs.Find<Subscription>
                                  |> fun x -> "{_id: 0}"
                                              |> ProjectionDefinition<Subscription, Subscription>.op_Implicit
                                              |> x.Project<Subscription>
                                  |> fun x -> x.ToListAsync() |> Async.AwaitTask

             let! myNewSubs = userId |> sprintf "{userId: \"%O\"}" 
                                     |> FilterDefinition.op_Implicit 
                                     |> newSubs.Find<NewSubscription>
                                     |> fun x -> "{_id: 0}"
                                                 |> ProjectionDefinition<NewSubscription, NewSubscription>.op_Implicit
                                                 |> x.Project<NewSubscription>
                                     |> fun x -> x.ToListAsync() |> Async.AwaitTask

             return UserSubscriptions (myNewSubs |> List.ofSeq, mySubs |> List.ofSeq)
         | AddNewSubscription (userId, uri) -> 
             let col = db.GetCollection<NewSubscription>("newSubscriptions")
             do! col.InsertOneAsync { userId = userId; uri = uri } |> Async.AwaitTask
             return Unit
         | _ -> return Unit
    } |> Async.StartAsTask

[<EntryPoint>]
let main argv =
    let bus = RabbitHutch.CreateBus("host=localhost")
    bus.RespondAsync<Command, Responses>(fun x -> handle x) |> ignore
    printfn "Waiting for commands..."
    0