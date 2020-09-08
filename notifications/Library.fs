module Spectator.Notifications

open Spectator.Core

type State =
    { users : Map<Subscription TypedId, UserId>
      queue : (UserId * string) list
      initialized : bool }

module Module2 =
    let formatSnapshot snap =
        match snap.uri.ToString() with
        | Regex "https://t.me/.+" [] -> sprintf "%O" snap.uri
        | _ -> sprintf "%s\n\n<a href=\"%O\">[ OPEN ]</a>" snap.title snap.uri

module StoreDomain =
    let init = { users = Map.empty; queue = []; initialized = false }

    let update state = function
        | SubscriptionCreated sub ->
            { state with users = Map.add sub.id sub.userId state.users }
        | SubscriptionRemoved (subId, _) ->
            { state with
                users = state.users
                        |> Map.filter @@ fun k _ -> not @@ List.contains k subId }
        | SnapshotCreated (true, snap) ->
            if state.initialized then
                match Map.tryFind snap.subscriptionId state.users with
                | Some userId ->
                    { state with queue = (userId, Module2.formatSnapshot snap) :: state.queue }
                | None ->
                    state
            else state
        | SnapshotCreated (false, _) -> state
        | NewSubscriptionCreated _ -> state

module Module1 =
    let main state =
        if state.initialized
            then { state with queue = [] }, [], state.queue
            else { state with queue = []; initialized = true }, [], []

let emptyState = StoreDomain.init
let restore = StoreDomain.update

let private localUpdate update f =
    async {
        let! db = update <| fun db -> (fst <| f db), []
        return snd <| f db
    }

let main sendToTelegramSingle (update : EffectReducer<_, _>) =
    async {
        let! updates = update.invoke Module1.main

        do! updates
            |> List.map (uncurry sendToTelegramSingle)
            |> Async.Sequential
            |> Async.Ignore

        do! Async.Sleep 1_000
    }
