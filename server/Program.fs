open EasyNetQ
open MongoDB.Driver
open Spectator.Core
open System

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
        |> DB.insert { userId = userId; uri = uri }
        |> Async.replaceWith SubscriptionCreatedSuccessfull

    let createSubscription (db : IMongoDatabase) userId uri provider = 
        async { 
            let newSubs = db.GetCollection<NewSubscription>("newSubscriptions")
            do! sprintf "{uri: \"%O\", userId: \"%s\"}" uri userId
                |> FilterDefinition.op_Implicit
                |> newSubs.DeleteManyAsync
                |> (Async.AwaitTask >> Async.Ignore)

            let subs = db.GetCollection<Subscription>("subscriptions")
            do! DB.insert { id = Guid.NewGuid(); userId = userId; provider = provider; uri = uri } subs

            return EmptyResponse
        }
    
    let addSnapshotsForSubscription (db : IMongoDatabase) snapshots = 
        db.GetCollection<Snapshot>("snapshots")
        |> DB.insertMany snapshots
        |> Async.replaceWith EmptyResponse

module Services =
    let executeCommand db cmd = 
        match cmd with
        | GetUserSubscriptions userId -> Repository.getUserSubscriptions db userId
        | AddNewSubscription(userId, uri) -> 
            Repository.addNewSubscription db userId uri
        | CreateSubscription (userId, uri, provider) -> Repository.createSubscription db userId uri provider
        | AddSnapshotsForSubscription(snapshots, subscription) -> 
            Repository.addSnapshotsForSubscription db snapshots
        | _ -> EmptyResponse |> Async.lift

[<EntryPoint>]
let main _ = 
    printfn "Server started..."
    let db = MongoClient("mongodb://localhost").GetDatabase("spectator")
    let bus = RabbitHutch.CreateBus("host=localhost;timeout=60")
    Bus.respondCommandAsync bus (Services.executeCommand db)
    0