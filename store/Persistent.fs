module Spectator.Store.Persistent

open MongoDB.Bson
open MongoDB.Bson.Serialization

type MongoCmd =
    | Insert of string * BsonDocument
    | Delete of string * System.Guid

module Domain =
    open Spectator.Core

    let collections = [ "subscriptions"; "snapshots" ]

    let restore name (doc: BsonDocument) =
        match name with
        | "snapshots" ->
            let (s: Snapshot) = BsonSerializer.Deserialize doc
            SnapshotCreated(false, s)
        | "subscriptions" ->
            let (s: Subscription) = BsonSerializer.Deserialize doc
            SubscriptionCreated s
        | _ -> failwithf "Unsupported collections %s" name

    let update e =
        match e with
        | SubscriptionCreated sub ->
            let ser =
                BsonSerializer.LookupSerializer<Subscription>()

            let doc = ser.ToBsonValue(sub) :?> BsonDocument
            [ Insert("subscriptions", doc) ]
        | SubscriptionRemoved (sids, _) ->
            sids
            |> List.map (fun id -> TypedId.unwrap id)
            |> List.map (fun id -> Delete("subscriptions", id))
        | SnapshotCreated (_, snap) ->
            let ser =
                BsonSerializer.LookupSerializer<Snapshot>()

            let doc = ser.ToBsonValue(snap) :?> BsonDocument
            [ Insert("snapshots", doc) ]
        | NewSubscriptionCreated _
        | HealthCheckRequested _ -> []

type State =
    { queue: MongoCmd list }
    static member Empty = { queue = [] }

let update state e =
    { state with
          queue = (Domain.update e) @ state.queue }

let main insert delete reducer =
    async {
        let! (db: State) = reducer (fun _ -> State.Empty, [])

        for e in (List.rev db.queue) do
            do! match e with
                | Insert (col, i) -> insert col i
                | Delete (col, id) -> delete col id
    }

let restore query recuder =
    async {
        for name in Domain.collections do
            do! query
                    name
                    (fun doc ->
                        let events = Domain.restore name doc
                        recuder (fun db -> db, [ events ]) |> Async.Ignore)
    }
