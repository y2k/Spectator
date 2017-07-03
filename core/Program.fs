open System
open EasyNetQ
open Spectator.Core
open System.Threading.Tasks
open MongoDB.Bson
open MongoDB.Driver

[<EntryPoint>]
let main argv =
    let bus = RabbitHutch.CreateBus("host=localhost")
    
    // bus.Respond<Requests, SubscriptionRequest list>(fun x -> [{ id = Guid.NewGuid(); url = Uri "http://google.com/ "}])
    // |> ignore

    bus.RespondAsync<Requests, SubscriptionRequest list>(
        fun x -> async {

                    let db = MongoClient("mongodb://localhost").GetDatabase("spec")
                    let users = db.GetCollection<User> "users"

                    return [{ id = Guid.NewGuid(); url = Uri "http://google.com/ "}]
                 } |> Async.StartAsTask) 
    |> ignore

    bus.SubscribeAsync<CreateSubscriptionCommand>(
        "", 
        fun x -> async { 
                    return () 
                 } |> Async.StartAsTask :> Task) |> ignore

    0