module Spectator.Server.App

open System
open EasyNetQ
open MongoDB.Driver
open Spectator.Core

module DB = Spectator.Server.Infrastructure.MongoDb

let SubscriptionsDb = "subscriptions"
let NewSubscriptionsDb = "newSubscriptions"

module Repository =
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

let start() : MailboxProcessor<Command> =
    printfn "Server started..."
    let db = MongoClient("mongodb://localhost").GetDatabase("spectator")
    MailboxProcessor.Start(fun inbox ->
        let rec loop() =
            async {
                let! cmd = inbox.Receive()
                match cmd with
                | AddSnapshotsForSubscription(snapshots, _, r) ->
                    do! Repository.addSnapshotsForSubscription db snapshots
                    r.Reply()
                | GetAllSubscriptions r -> let! subs = DB.findWithoutId'<Subscription> db SubscriptionsDb
                                           r.Reply subs
                | CreateSubscription(userId, uri, provider, r) ->
                    do! Repository.createSubscription db userId uri provider
                    r.Reply()
                | GetAllNewSubscriptions r -> let! subs = DB.findWithoutId'<NewSubscription> db NewSubscriptionsDb
                                              r.Reply subs
                | GetUserSubscriptions(userId, r) -> let! mySubs = Repository.getSubscriptions db userId
                                                     let! myNewSubs = Repository.getNewSubscriptions db userId
                                                     r.Reply(myNewSubs, mySubs)
                | AddNewSubscription(userId, uri, r) ->
                    do! Repository.addNewSubscription db userId uri
                    r.Reply()
                | _ -> ()
                do! loop()
            }
        loop())

[<EntryPoint>]
let main _ = failwith "TODO"
