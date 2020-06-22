module Spectator.Store.Persistent

open Spectator.Core

type IInsert =
    abstract invoke : string -> 'a -> unit Async

type IForEach =
    abstract invoke : string -> ('a -> unit)-> unit Async

let restoreState (forEach : IForEach) emptyState f =
    async {
        let state = ref emptyState
        let update e = state := f !state e

        do! forEach.invoke "subscriptions" (fun s -> update <| SubscriptionCreated s)
        do! forEach.invoke "snapshots" (fun s -> update <| SnapshotCreated s)

        return !state
    }

let executeEffect<'a> (insert : IInsert) delete event =
    async {
        match event with
        | SubscriptionCreated sub ->
            do! insert.invoke "subscriptions" sub
        | SubscriptionRemoved (sids, _) ->
            for id in sids do
                let id : System.Guid = TypedId.unwrap id
                do! delete "subscriptions" id
        | SnapshotCreated snap ->
            do! insert.invoke "snapshots" snap
        | NewSubscriptionCreated _ -> ()
        return []
    }

let main insert delete receiveEvent =
    Tea.start () []
        (fun s _ -> (), List.map (executeEffect insert delete) s)
        List.singleton id receiveEvent
