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
             let! mySubs = userId |> sprintf "{userId: \"%O\"}" |> FilterDefinition.op_Implicit 
                                  |> subs.FindAsync<Subscription> |> Async.AwaitTask
             let! myNewSubs = userId |> sprintf "{userId: \"%O\"}" |> FilterDefinition.op_Implicit 
                                     |> newSubs.FindAsync<NewSubscription> |> Async.AwaitTask
             let! mySubsList = mySubs.ToListAsync() |> Async.AwaitTask
             let! myNewSubsList = myNewSubs.ToListAsync() |> Async.AwaitTask
             return UserSubscriptions (myNewSubsList |> List.ofSeq, mySubsList |> List.ofSeq)
         | AddNewSubscription (userId, uri) -> 
             let col = db.GetCollection<NewSubscription>("newSubscriptions")
             do! col.InsertOneAsync { userId = userId; uri = uri } |> Async.AwaitTask
             return Unit
         | _ -> return Unit
    } |> Async.StartAsTask

[<EntryPoint>]
let main argv =
    async {
        let! result = handle (GetUserSubscriptions "241854720") |> Async.AwaitTask
        printfn "Result = %O" result
    } |> Async.RunSynchronously

    // let bus = RabbitHutch.CreateBus("host=localhost")
    // bus.RespondAsync<Command, Responses>(fun x -> handle x) |> ignore
    // printfn "Waiting for commands..."
    0