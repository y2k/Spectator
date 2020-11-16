module Spectator.Notifications

open Spectator.Core

type State =
    { users: Map<Subscription TypedId, UserId>
      queue: (UserId * string) list
      initialized: bool }
    static member Empty =
        { users = Map.empty
          queue = []
          initialized = false }

module StoreDomain =
    let formatSnapshot snap =
        match snap.uri.ToString() with
        | Regex "https://t.me/.+" [] -> sprintf "%O" snap.uri
        | _ -> sprintf "%s\n\n<a href=\"%O\">[ OPEN ]</a>" snap.title snap.uri

    let update state =
        function
        | SubscriptionCreated sub ->
            { state with
                  users = Map.add sub.id sub.userId state.users }
        | SubscriptionRemoved (subId, _) ->
            { state with
                  users =
                      state.users
                      |> Map.filter (fun k _ -> not <| List.contains k subId) }
        | SnapshotCreated (true, snap) ->
            if state.initialized then
                match Map.tryFind snap.subscriptionId state.users with
                | Some userId ->
                    { state with
                          queue = (userId, formatSnapshot snap) :: state.queue }
                | None -> state
            else
                state
        | SnapshotCreated (false, _) -> state
        | NewSubscriptionCreated _
        | HealthCheckRequested _ -> state

    let clearQueue state =
        if state.initialized then
            { state with queue = [] }, []
        else
            { state with
                  queue = []
                  initialized = true },
            []

    let getUpdates state =
        if state.initialized then state.queue else []

let restore = StoreDomain.update

let main sendToTelegramSingle update =
    async {
        let! updates =
            update StoreDomain.clearQueue
            >>- StoreDomain.getUpdates

        do! updates
            |> List.map (uncurry sendToTelegramSingle)
            |> Async.Sequential
            |> Async.Ignore

        do! Async.Sleep 1_000
    }
