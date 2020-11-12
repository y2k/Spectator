module Spectator.Store.Persistent

open Spectator.Core

module P = Tea.Persistent
module M = MongoDb

let applyEvent db e =
    async {
        match e with
        | SubscriptionCreated sub -> do! M.insert db "subscriptions" sub
        | SubscriptionRemoved (sids, _) ->
            for id in sids do
                let id: System.Guid = TypedId.unwrap id
                do! M.delete db "subscriptions" id
        | SnapshotCreated (_, snap) -> do! M.insert db "snapshots" snap
        | NewSubscriptionCreated _
        | HealthCheckRequested _ -> ()
    }

let applyObj db update =
    async {
        do! M.forEach db "subscriptions" (fun (s: Subscription) -> update <| SubscriptionCreated s)
        do! M.forEach db "snapshots" (fun (s: Snapshot) -> update <| SnapshotCreated(false, s))
    }
