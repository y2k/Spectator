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
            db.GetCollection<Subscription>("subscriptions") 
            |> DB.findWithoutId (sprintf "{userId: \"%s\"}" userId)
        let myNewSubs = 
            db.GetCollection<NewSubscription>("newSubscriptions") 
            |> DB.findWithoutId (sprintf "{userId: \"%s\"}" userId)
        Async.zip myNewSubs mySubs UserSubscriptions
    
    let addNewSubscription (db : IMongoDatabase) userId uri = 
        db.GetCollection<NewSubscription>("newSubscriptions")
        |> DB.insert { userId = userId
                       uri = uri }
        |> Async.replaceWith SubscriptionCreatedSuccessfull
    
    let createSubscriptions (db : IMongoDatabase) = 
        async { 
            let subs = db.GetCollection<Subscription>("subscriptions")
            let newSubs = db.GetCollection<NewSubscription>("newSubscriptions")
            // FIXME:
            return Unit
        }
    
    let addSnapshotsForSubscription (db : IMongoDatabase) snapshots = 
        db.GetCollection<Snapshot>("snapshots")
        |> DB.insertMany snapshots
        |> Async.replaceWith Unit

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