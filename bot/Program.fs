module Spectator.Bot.App

open System
open Spectator.Core

module R = Spectator.Server.App.Repository

type Cmd =
    | GetUserSubscriptionsCmd of UserId
    | UnknownCmd
    | AddNewSubscriptionCmd of UserId * Uri

type CollectionName = string

type MongoDbFilter = string

type MongoDbEffect =
    | WriteDb of CollectionName * value : obj
    | ReadDb of (CollectionName * MongoDbFilter) list

module Domain =
    open MongoDB.Bson
    open MongoDB.Bson.Serialization

    let parse (message : Bot.Message) =
        match String.split message.text ' ' with
        | "/ls" :: _ -> GetUserSubscriptionsCmd message.user
        | "/add" :: url :: _ -> AddNewSubscriptionCmd(message.user, Uri url)
        | _ -> UnknownCmd

    let subListToMessageResponse (newSubs : NewSubscription list) (subs : Subscription list) =
        newSubs
        |> List.map (fun x -> sprintf "(Waiting) %O" x.uri)
        |> List.append (subs |> List.map (fun x -> string x.uri))
        |> List.fold (sprintf "%s\n- %s") "Your subscriptions: "

    let handle message =
        match parse message with
        | GetUserSubscriptionsCmd userId ->
            ReadDb [ R.SubscriptionsDb, (sprintf "{userId: \"%s\"}" userId)
                     R.NewSubscriptionsDb, (sprintf "{userId: \"%s\"}" userId) ],
            fun (xs : BsonDocument list list) ->
                let mySubs = xs.[0] |> List.map BsonSerializer.Deserialize<Subscription>
                let myNewSubs = xs.[1] |> List.map BsonSerializer.Deserialize<NewSubscription>
                subListToMessageResponse myNewSubs mySubs
        | AddNewSubscriptionCmd(userId, uri) ->
            WriteDb(R.NewSubscriptionsDb,
                    { userId = userId
                      uri = uri }), fun _ -> "Your subscription created"
        | UnknownCmd -> ReadDb [], fun _ -> "/ls - show your subscriptions\n/add [url] - add new subscription"

module Services =
    open MongoDB
    open MongoDB.Driver
    open MongoDB.Bson

    let readList (x : IFindFluent<BsonDocument, BsonDocument>) =
        x.Project(ProjectionDefinition<_, _>.op_Implicit "{_id: 0}").ToListAsync()
        |> Async.AwaitTask
        >>- Seq.toList

    let handleTelegramMessage (db : IMongoDatabase) message =
        async {
            printfn "Incoming msg : %O" message
            let (cmd, callback) = Domain.handle message
            match cmd with
            | WriteDb(collection, value) ->
                let col = db.GetCollection collection
                do! col.InsertOneAsync value |> Async.AwaitTask
                return callback []
            | ReadDb readers ->
                let! xs = readers
                          |> List.map (fun (collection, filter) ->
                                 let col = db.GetCollection<BsonDocument> collection
                                 col.Find(FilterDefinition.op_Implicit filter) |> readList)
                          |> Async.Parallel
                          >>- List.ofArray
                return callback xs
        }

let start db = async { Bot.repl (Services.handleTelegramMessage db) }

[<EntryPoint>]
let main _ = failwith "TODO"
