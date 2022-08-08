module Spectator.Store.Persistent

open Spectator.Core
open LiteDB

type MongoCmd =
    | Insert of string * BsonDocument
    | Delete of string * System.Guid

module Domain =
    let collections = [ "subscriptions"; "snapshots" ]

    let restore name (doc: BsonDocument) : Event =
        match name with
        | "snapshots" ->
            let (s: Snapshot) = BsonMapper.Global.Deserialize doc
            SnapshotCreated(false, s)
        | "subscriptions" ->
            let (s: Subscription) = BsonMapper.Global.Deserialize doc
            SubscriptionCreated s
        | _ -> failwithf "Unsupported collections %s" name

    let update (e: Event) =
        match e with
        | :? SubscriptionCreated as SubscriptionCreated sub ->
            let doc = BsonMapper.Global.Serialize sub
            [ Insert("subscriptions", doc :?> BsonDocument) ]
        | :? SnapshotCreated as SnapshotCreated (_, snap) ->
            let doc = BsonMapper.Global.Serialize snap
            [ Insert("snapshots", doc :?> BsonDocument) ]
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

let handleCommand (chan: MongoCmd AsyncChannel.t) (cmd: Command) =
    Domain.update cmd
    |> List.iter (AsyncChannel.write chan)

let main (dbAdapter: DatabaseAdapter.t) (chan: MongoCmd AsyncChannel.t) =
    async {
        while true do
            let! e = AsyncChannel.read chan

            do!
                match e with
                | Insert (col, i) -> DatabaseAdapter.insert dbAdapter col i
                | Delete (col, id) -> DatabaseAdapter.delete dbAdapter col id
    }

// let main (dbAdapter: DatabaseAdapter.t) (reducer: IReducer<_, Event>) =
//     async {
//         let! (db: State) = reducer.Invoke(fun db -> State.Empty, [], db)

//         for e in (List.rev db.queue) do
//             do!
//                 match e with
//                 | Insert (col, i) -> DatabaseAdapter.insert dbAdapter col i
//                 | Delete (col, id) -> DatabaseAdapter.delete dbAdapter col id
//     }

let restore (db: DatabaseAdapter.t) dispatch =
    async {
        for name in Domain.collections do
            do!
                DatabaseAdapter.queryAll db name (fun doc ->
                    let events = Domain.restore name doc
                    dispatch events
                    async.Return())
    }
