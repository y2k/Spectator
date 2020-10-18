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

        do! forEach.invoke "subscriptions" (fun (s : Subscription) -> update <| SubscriptionCreated s)
        do! forEach.invoke "snapshots" (fun (s : Snapshot) -> update <| SnapshotCreated (false, s))

        return !state
    }

type State = { queue : Events list }

let initState = { queue = [] }

let restore s e = { queue = e :: s.queue}

let main (insert : IInsert) delete  (reducer : EffectReducer<_, _>) =
    async {
        let! queue = reducer.invoke @@ fun db -> { queue = []}, [], db.queue
        for e in queue |> List.rev do
            match e with
            | SubscriptionCreated sub ->
                do! insert.invoke "subscriptions" sub
            | SubscriptionRemoved (sids, _) ->
                for id in sids do
                    let id : System.Guid = TypedId.unwrap id
                    do! delete "subscriptions" id
            | SnapshotCreated (_, snap) ->
                do! insert.invoke "snapshots" snap
            | NewSubscriptionCreated _ | HealthCheckRequested _ -> ()

        do! Async.Sleep 1_000
    }

let executeEffect<'a> (insert : IInsert) delete event : 'a list Async =
    async {
        match event with
        | SubscriptionCreated sub ->
            do! insert.invoke "subscriptions" sub
        | SubscriptionRemoved (sids, _) ->
            for id in sids do
                let id : System.Guid = TypedId.unwrap id
                do! delete "subscriptions" id
        | SnapshotCreated (_, snap) ->
            do! insert.invoke "snapshots" snap
        | NewSubscriptionCreated _ | HealthCheckRequested _ -> ()
        return []
    }
