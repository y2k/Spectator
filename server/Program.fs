module Spectator.Server.App

open EasyNetQ
open MongoDB.Driver
open Spectator.Core
open System

module DB = Spectator.Server.Infrastructure.MongoDb
module Bus = Spectator.Server.Infrastructure.Bus

module Repository =
    let getUserSubscriptions' (db : IMongoDatabase) userId =
        let mySubs =
            db.GetCollection<Subscription>("subscriptions") |> DB.findWithoutId (sprintf "{userId: \"%s\"}" userId)
        let myNewSubs =
            db.GetCollection<NewSubscription>("newSubscriptions")
            |> DB.findWithoutId (sprintf "{userId: \"%s\"}" userId)
        Async.zip myNewSubs mySubs id

    let addNewSubscription' (db : IMongoDatabase) userId uri =
        db.GetCollection<NewSubscription>("newSubscriptions")
        |> DB.insert { userId = userId
                       uri = uri }

    let createSubscription' (db : IMongoDatabase) userId uri provider =
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

    let addSnapshotsForSubscription' (db : IMongoDatabase) snapshots =
        db.GetCollection<Snapshot>("snapshots") |> DB.insertMany snapshots

let start() : MailboxProcessor<Command> =
    printfn "Server started..."
    let db = MongoClient("mongodb://localhost").GetDatabase("spectator")
    MailboxProcessor.Start(fun inbox ->
        let rec loop() =
            async {
                let! cmd = inbox.Receive()
                match cmd with
                | AddSnapshotsForSubscription'(snapshots, _, r) ->
                    do! Repository.addSnapshotsForSubscription' db snapshots
                    r.Reply()
                | GetSubscriptions' _ -> failwith "TODO"
                | CreateSubscription'(userId, uri, provider, r) ->
                    do! Repository.createSubscription' db userId uri provider
                    r.Reply()
                | GetNewSubscriptions' _ -> failwith "TODO"
                | GetUserSubscriptions'(userId, r) -> let! xs = Repository.getUserSubscriptions' db userId
                                                      r.Reply xs
                | AddNewSubscription'(userId, uri, r) ->
                    do! Repository.addNewSubscription' db userId uri
                    r.Reply()
                | _ -> ()
                do! loop()
            }
        loop())

[<EntryPoint>]
let main _ =
    failwith "TODO"
    0
