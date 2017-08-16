open System
open System.Threading.Tasks
open EasyNetQ
open MongoDB.Bson
open MongoDB.Driver
open Spectator.Core

module Async = Spectator.Infrastructure.Async
module DB = Spectator.Infrastructure.MongoDb
module Bus = Spectator.Infrastructure.Bus

module Repository = 
    let getUserSubscriptions (db : IMongoDatabase) userId = 
        let mySubs = 
            sprintf "{userId: \"%s\"}" userId 
            |> DB.findWithoutId 
                   (db.GetCollection<Subscription>("subscriptions"))
        let myNewSubs = 
            sprintf "{userId: \"%s\"}" userId 
            |> DB.findWithoutId 
                   (db.GetCollection<NewSubscription>("newSubscriptions"))
        Async.zip myNewSubs mySubs UserSubscriptions
    
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
    Bus.respondCommandAsync bus (executeCommand db)
    printfn "Waiting for commands..."
    0