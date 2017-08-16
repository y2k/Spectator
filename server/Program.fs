open System
open System.Threading.Tasks
open EasyNetQ
open MongoDB.Bson
open MongoDB.Driver
open Spectator.Core

module Repository = 
    let getUserSubscriptions (db : IMongoDatabase) userId = 
        let subs = db.GetCollection<Subscription>("subscriptions")
        let newSubs = db.GetCollection<NewSubscription>("newSubscriptions")
        async { 
            let! mySubs = userId
                          |> sprintf "{userId: \"%s\"}"
                          |> FilterDefinition.op_Implicit
                          |> subs.Find<Subscription>
                          |> fun x -> 
                              "{_id: 0}"
                              |> ProjectionDefinition<_, _>.op_Implicit
                              |> x.Project<Subscription>
                          |> fun x -> x.ToListAsync() |> Async.AwaitTask
            let! myNewSubs = userId
                             |> sprintf "{userId: \"%s\"}"
                             |> FilterDefinition.op_Implicit
                             |> newSubs.Find<NewSubscription>
                             |> fun x -> 
                                 "{_id: 0}"
                                 |> ProjectionDefinition<_, _>.op_Implicit
                                 |> x.Project<NewSubscription>
                             |> fun x -> x.ToListAsync() |> Async.AwaitTask
            return UserSubscriptions
                       (myNewSubs |> List.ofSeq, mySubs |> List.ofSeq)
        }
    
    let addNewSubscription (db : IMongoDatabase) userId uri = 
        async { 
            let col = db.GetCollection<NewSubscription>("newSubscriptions")
            do! col.InsertOneAsync { userId = userId
                                     uri = uri }
                |> Async.AwaitTask
            return SubscriptionCreatedSuccessfull
        }
    
    let createSubscriptions (db : IMongoDatabase) = 
        async { 
            let subs = db.GetCollection<Subscription>("subscriptions")
            let newSubs = db.GetCollection<NewSubscription>("newSubscriptions")
            // FIXME:
            return Unit
        }
    
    let addSnapshotsForSubscription (db : IMongoDatabase) snapshots = 
        async { 
            let subs = db.GetCollection<Snapshot>("snapshots")
            do! subs.InsertManyAsync(snapshots) |> Async.AwaitTask
            return Unit
        }

let executeCommand db cmd = 
    match cmd with
    | GetUserSubscriptions userId -> Repository.getUserSubscriptions db userId
    | AddNewSubscription(userId, uri) -> 
        Repository.addNewSubscription db userId uri
    | CreateSubscriptions subsWithProv -> Repository.createSubscriptions db
    | AddSnapshotsForSubscription(snapshots, subscription) -> 
        Repository.addSnapshotsForSubscription db snapshots
    | _ -> Unit |> async.Return

[<EntryPoint>]
let main argv = 
    let db = MongoClient("mongodb://localhost").GetDatabase("spectator")
    let bus = RabbitHutch.CreateBus("host=localhost")
    bus.RespondAsync<Command, Responses>
        (fun x -> executeCommand db x |> Async.StartAsTask) |> ignore
    printfn "Waiting for commands..."
    0