module Spectator.Server.App

module Repository =
    open System
    open MongoDB.Driver
    open Spectator.Core

    module DB = Spectator.Server.Infrastructure.MongoDb

    let SubscriptionsDb = "subscriptions"
    let NewSubscriptionsDb = "newSubscriptions"

    let getSubscriptions' db = DB.findWithoutId'<Subscription> db SubscriptionsDb

    let getNewSubscriptions' db = DB.findWithoutId'<NewSubscription> db NewSubscriptionsDb

    let getSubscriptions (db : IMongoDatabase) userId =
        db.GetCollection<Subscription>(SubscriptionsDb) |> DB.findWithoutId (sprintf "{userId: \"%s\"}" userId)

    let getNewSubscriptions (db : IMongoDatabase) userId =
        db.GetCollection<NewSubscription>(NewSubscriptionsDb) |> DB.findWithoutId (sprintf "{userId: \"%s\"}" userId)

    let addNewSubscription (db : IMongoDatabase) userId uri =
        db.GetCollection<NewSubscription>(NewSubscriptionsDb)
        |> DB.insert { userId = userId
                       uri = uri }

    let createSubscription (db : IMongoDatabase) userId uri provider =
        async {
            let newSubs = db.GetCollection<NewSubscription>("newSubscriptions")
            do! sprintf "{uri: \"%O\", userId: \"%s\"}" uri userId
                |> FilterDefinition.op_Implicit
                |> newSubs.DeleteManyAsync
                |> (Async.AwaitTask >> Async.Ignore)
            let subs = db.GetCollection<Subscription>("subscriptions")
            do! DB.insert { id = Guid.NewGuid()
                            userId = userId
                            provider = provider
                            uri = uri } subs
        }

    let addSnapshotsForSubscription (db : IMongoDatabase) snapshots =
        db.GetCollection<Snapshot>("snapshots") |> DB.insertMany snapshots

[<EntryPoint>]
let main _ = failwith "TODO"
