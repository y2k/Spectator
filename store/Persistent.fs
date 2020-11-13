module Spectator.Store.Persistent

open Spectator.Core
open MongoDB.Bson
open MongoDB.Bson.Serialization

type MongoCmd =
    | Insert of string * BsonDocument
    | Delete of string * System.Guid

type State =
    { queue: MongoCmd list }
    static member Empty = { queue = [] }

let restore' name (doc: BsonDocument) =
    match name with
    | "snapshots" ->
        let (s: Snapshot) = BsonSerializer.Deserialize doc
        SnapshotCreated(false, s)
    | "subscriptions" ->
        let (s: Subscription) = BsonSerializer.Deserialize doc
        SubscriptionCreated s
    | _ -> failwithf "Unsupported collections %s" name

let update state e =
    match e with
    | SubscriptionCreated sub ->
        let ser =
            BsonSerializer.LookupSerializer<Subscription>()

        let doc = ser.ToBsonValue(sub) :?> BsonDocument
        let xs = [ Insert("subscriptions", doc) ]
        { state with queue = xs @ state.queue }
    | SubscriptionRemoved (sids, _) ->
        let xs =
            sids
            |> List.map (fun id -> TypedId.unwrap id)
            |> List.map (fun id -> Delete("subscriptions", id))

        { state with queue = xs @ state.queue }
    | SnapshotCreated (_, snap) ->
        let ser =
            BsonSerializer.LookupSerializer<Snapshot>()

        let doc = ser.ToBsonValue(snap) :?> BsonDocument
        let xs = [ Insert("snapshots", doc) ]
        { state with queue = xs @ state.queue }
    | NewSubscriptionCreated _
    | HealthCheckRequested _ -> state

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
        for name in [ "subscriptions"; "snapshots" ] do
            do! query
                    name
                    (fun doc ->
                        let events = restore' name doc
                        recuder (fun db -> db, [ events ]) |> Async.Ignore)
    }
