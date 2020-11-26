module Spectator.Notifications

open Spectator.Core

type State =
    { users: Map<Subscription TypedId, UserId>
      queue: (UserId * Snapshot) list
      initialized: bool }
    static member Empty =
        { users = Map.empty
          queue = []
          initialized = false }

module State =
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
                          queue = (userId, snap) :: state.queue }
                | None -> state
            else
                state
        | SnapshotCreated (false, _) -> state
        | NewSubscriptionCreated _
        | HealthCheckRequested _ -> state

module private Domain =
    let clearQueue state =
        if state.initialized then
            { state with queue = [] }, []
        else
            { state with
                  queue = []
                  initialized = true },
            []

    let getUpdates state =
        let formatSnapshot snap =
            match snap.uri.ToString() with
            | Regex "https://t.me/.+" [] -> sprintf "%O" snap.uri
            | _ -> sprintf "%s\n\n<a href=\"%O\">[ OPEN ]</a>" snap.title snap.uri

        if state.initialized then state.queue else []
        |> List.map (fun (u, s) -> u, formatSnapshot s)

let main sendToTelegramSingle update =
    async {
        let! updates = update Domain.clearQueue >>- Domain.getUpdates

        do! updates
            |> List.map (uncurry sendToTelegramSingle)
            |> Async.Sequential
            |> Async.Ignore
    }
