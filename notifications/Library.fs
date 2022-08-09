module Spectator.Notifications

open Spectator.Core

type State =
    { users: Map<Subscription TypedId, UserId>
      queue: (UserId * Snapshot) list
      initialized: bool }
    interface Command

    static member empty =
        { users = Map.empty
          queue = []
          initialized = false }

module private State =
    let update state (event: Command) =
        match event with
        | :? SnapshotCreated as SnapshotCreated (true, snap) ->
            if state.initialized then
                match Map.tryFind snap.subscriptionId state.users with
                | Some userId -> { state with queue = (userId, snap) :: state.queue }
                | None -> state
            else
                state
        | :? SnapshotCreated as SnapshotCreated (false, _) -> state
        | :? SubscriptionCreated as SubscriptionCreated sub ->
            { state with users = Map.add sub.id sub.userId state.users }
        | :? SubscriptionRemoved as SubscriptionRemoved (subId, _) ->
            { state with
                users =
                    state.users
                    |> Map.filter (fun k _ -> not <| List.contains k subId) }
        | _ -> state

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

        if state.initialized then
            state.queue
        else
            []
        |> List.map (fun (u, s) -> u, formatSnapshot s)

    let execute state =
        let (state', events) = clearQueue state
        let result = getUpdates state
        state', events, result

let handleStateCmd (state: State) (cmd: Command) = State.update state cmd

let handleEvent (state: State) (e: Event) =
    match e with
    | :? TimerTicked ->
        let (newState, _, messages) = Domain.execute state

        [ yield newState :> Command
          yield! List.map (fun x -> let a = SendTelegramMessage x in a :> Command) messages ]
    | _ -> []
