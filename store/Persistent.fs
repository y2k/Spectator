module Spectator.Store.Persistent

open Spectator.Core
open LiteDB

type MongoCmd =
    private
    | Insert of string * BsonDocument
    | Delete of string * System.Guid

module private Domain =
    let collections = [ "subscriptions"; "snapshots" ]

    let restore name (doc: BsonDocument) : Command =
        match name with
        | "snapshots" ->
            let (s: Snapshot) = BsonMapper.Global.Deserialize doc
            SnapshotCreated(false, s)
        | "subscriptions" ->
            let (s: Subscription) = BsonMapper.Global.Deserialize doc
            SubscriptionCreated s
        | _ -> failwithf "Unsupported collections %s" name

    let update (e: Command) =
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

let private update state e =
    { state with queue = (Domain.update e) @ state.queue }

type t =
    private
        { chan: MongoCmd AsyncChannel.t
          db: DatabaseAdapter.t
          mutable restored: bool }

let make connectionString : t =
    { chan = AsyncChannel.make ()
      db = DatabaseAdapter.make connectionString
      restored = false }

type private MarkRestoreComplete =
    | MarkRestoreComplete
    interface Command

let handleCommand (t: t) (cmd: Command) =
    match cmd with
    | :? MarkRestoreComplete -> t.restored <- true
    | _ when t.restored -> Domain.update cmd |> List.iter (AsyncChannel.write t.chan)
    | _ -> ()

let main (t: t) =
    async {
        while true do
            let! e = AsyncChannel.read t.chan

            do!
                match e with
                | Insert (col, i) -> DatabaseAdapter.insert t.db col i
                | Delete (col, id) -> DatabaseAdapter.delete t.db col id
    }

let restoreCommand t dispatch =
    async {
        for name in Domain.collections do
            do!
                DatabaseAdapter.queryAll t.db name (fun doc ->
                    let cmd = Domain.restore name doc
                    dispatch cmd
                    async.Return())

        dispatch MarkRestoreComplete
    }
