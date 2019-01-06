module Spectator.Worker.App

open Spectator.Core

module R = Spectator.Server.App.Repository
module I = Spectator.Worker.Infrastructure

type CollectionName = string
type MongoDbFilter = string

module Domain'' =
    open MongoDB.Bson
    open MongoDB.Bson.Serialization

    type Msg = 
        | InitMsg 
        | NewSubLoadedMsg of BsonDocument list list
        | IsValidResultMsg of NewSubscription * bool
        | EmptyMsg
    type Cmd = 
        | ReadCmd of (CollectionName * MongoDbFilter) list * (BsonDocument list list -> Msg)
        | ProviderIsValidCmd of Provider * string * (bool -> Msg)
        | ActionDbCmd of (string * obj) list * (string * string) list * Msg
        | EmptyCmd

    let handle = 
        function
        | InitMsg -> 
            ReadCmd ( [R.NewSubscriptionsDb, null], NewSubLoadedMsg )
        | NewSubLoadedMsg xs -> 
            let subs = xs.[0] |> List.map BsonSerializer.Deserialize<NewSubscription>
            let sub = subs.[0]
            ProviderIsValidCmd ( Provider.Rss, string sub.uri, curry IsValidResultMsg sub)
        | IsValidResultMsg (sub, isValid) -> 
            if isValid then
                let x = { id = System.Guid.NewGuid () // FIXME:
                          userId = sub.userId
                          provider = Provider.Rss
                          uri = sub.uri }
                let save = [ R.SubscriptionsDb, x :> obj ]
                let delete = [ R.NewSubscriptionsDb, (sprintf "{uri: \"%O\"}" (failwith "???")) ]
                ActionDbCmd ( save, delete, EmptyMsg )
            else
                failwith "???"
        | EmptyMsg -> EmptyCmd

type MongoDbEffect =
    | ProviderIsValidEffect of Provider * string * (bool -> MongoDbEffect)
    | ReadEffect of (CollectionName * MongoDbFilter) list * (MongoDB.Bson.BsonDocument list list -> MongoDbEffect)
    | GroupEffect of (CollectionName * obj) list * (CollectionName * MongoDbFilter) list * (unit -> MongoDbEffect)
    | NoneEffect

module Domain =
    open MongoDB.Bson.Serialization

    let handle'' (sub : NewSubscription) isValid =
        if isValid then 
            let x = { id = System.Guid.NewGuid () // FIXME:
                      userId = sub.userId
                      provider = Provider.Rss
                      uri = sub.uri }
            let del = R.NewSubscriptionsDb, (sprintf "{uri: \"%O\"}" sub.uri)
            let wr = R.SubscriptionsDb, x :> obj
            GroupEffect ([ wr ], [ del ], always NoneEffect)
        else NoneEffect

    let handle' (xs : MongoDB.Bson.BsonDocument list list) =
        let subs = xs.[0] |> List.map BsonSerializer.Deserialize<NewSubscription>
        let sub = subs.[0]
        ProviderIsValidEffect (Provider.Rss, string sub.uri, handle'' sub)

    let handle =
        ReadEffect ([ R.NewSubscriptionsDb, null ], handle')

module Services =
    open MongoDB
    open MongoDB.Driver
    open MongoDB.Bson
    open MongoDB.Bson.Serialization

    let createNewSubscriptions (db : IMongoDatabase) =
        let rec executeEffects eff =
            let readList (x : IFindFluent<BsonDocument, BsonDocument>) =
                x.Project(ProjectionDefinition<_, _>.op_Implicit "{_id: 0}").ToListAsync()
                |> Async.AwaitTask
                >>- Seq.toList
            async {
                match eff with 
                | ProviderIsValidEffect (_, _, _) -> failwith "???"
                | ReadEffect (readers, callback) -> 
                    let! xs = readers
                            |> List.map (fun (collection, filter) ->
                                    let col = db.GetCollection<BsonDocument> collection
                                    col.Find(FilterDefinition.op_Implicit filter) |> readList)
                            |> Async.Parallel
                            >>- List.ofArray
                    do! executeEffects <| callback xs
                | GroupEffect (ws, _, callback) -> 
                    do! ws 
                        |> List.map (fun (collection, value) -> 
                                        let col = db.GetCollection collection
                                        col.InsertOneAsync value |> Async.AwaitTask)
                        |> Async.Parallel 
                        |> Async.Ignore
                    do! executeEffects <| callback ()
                | NoneEffect -> ()
            }
        executeEffects Domain.handle

    let private getNodesWithSubscription (x : Subscription) = RssParser.getNodes x.uri >>- fun snaps -> snaps, x

    let loadNewSnapshot db =
        R.getSubscriptions' db >>- List.filter (fun x -> x.provider = Provider.Rss)
        |> Async.bindAll getNodesWithSubscription
        |> Async.bindAll (fun (snapshots, subId) -> R.addSnapshotsForSubscription db snapshots)
        >>- ignore

let start db =
    async {
        printfn "Start worker..."
        Services.createNewSubscriptions db *> Services.loadNewSnapshot db |> I.executeInLoop 10000
    }

[<EntryPoint>]
let main _ = failwith "TODO"
