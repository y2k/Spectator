module Spectator.Store.Persistent

open Spectator.Core
open MongoDB.Bson
open MongoDB.Bson.Serialization

type MongoCmd =
    | Insert of string * BsonDocument
    | Delete of string * System.Guid

module Domain =
    let collections = [ "subscriptions"; "snapshots" ]

    let restore name (doc: BsonDocument) : Event =
        match name with
        | "snapshots" ->
            let (s: Snapshot) = BsonSerializer.Deserialize doc
            SnapshotCreated(false, s)
        | "subscriptions" ->
            let (s: Subscription) = BsonSerializer.Deserialize doc
            SubscriptionCreated s
        | _ -> failwithf "Unsupported collections %s" name

    let update (e: Event) =
        match e with
        | :? SubscriptionCreated as SubscriptionCreated sub ->
            let ser = BsonSerializer.LookupSerializer<Subscription>()

            let doc = ser.ToBsonValue(sub) :?> BsonDocument
            [ Insert("subscriptions", doc) ]
        | :? SnapshotCreated as SnapshotCreated (_, snap) ->
            let ser = BsonSerializer.LookupSerializer<Snapshot>()

            let doc = ser.ToBsonValue(snap) :?> BsonDocument
            [ Insert("snapshots", doc) ]
        | :? SubscriptionRemoved as SubscriptionRemoved (sids, _) ->
            sids
            |> List.map (fun id -> TypedId.unwrap id)
            |> List.map (fun id -> Delete("subscriptions", id))
        | _ -> []

type State =
    { queue: MongoCmd list }
    static member Empty = { queue = [] }

let update state e =
    { state with queue = (Domain.update e) @ state.queue }

let main insert delete (reducer: IReducer<_, Event>) =
    async {
        let! (db: State) = reducer.Invoke(fun db -> State.Empty, [], db)

        for e in (List.rev db.queue) do
            do!
                match e with
                | Insert (col, i) -> insert col i
                | Delete (col, id) -> delete col id
    }

let restore query (reducer: IReducer<_, Event>) =
    async {
        for name in Domain.collections do
            do!
                query name (fun doc ->
                    let events = Domain.restore name doc
                    reducer.Invoke(fun db -> db, [ events ], ()))
    }
