module Spectator.Notifications

open Spectator.Core
open Y2k.EventBus

type State =
    { users: Map<Subscription TypedId, UserId>
      queue: (UserId * Snapshot) list
      initialized: bool }
    interface Command

    static member empty =
        { users = Map.empty
          queue = []
          initialized = false }

let handleStateCmd (state: State) (cmd: Command) =
    match cmd with
    | :? SnapshotCreated as SnapshotCreated (true, snap) ->
        if state.initialized then
            match Map.tryFind snap.subscriptionId state.users with
            | Some userId -> { state with queue = (userId, snap) :: state.queue }
            | None -> state
        else
            state
    | :? SubscriptionCreated as SubscriptionCreated sub -> { state with users = Map.add sub.id sub.userId state.users }
    | :? SubscriptionRemoved as SubscriptionRemoved (subId, _) ->
        { state with users = state.users |> Map.filter (fun k _ -> not <| List.contains k subId) }
    | _ -> state

let handleEvent (state: State) (_: Initialize) : Command list =
    let formatSnapshot snap =
        match string snap.uri with
        | Regex "https://t.me/.+" [] -> sprintf "%O" snap.uri
        | _ -> sprintf "%s\n\n<a href=\"%O\">[ OPEN ]</a>" snap.title snap.uri

    let newState =
        { state with
            queue = []
            initialized = true }

    let messages =
        if state.initialized then state.queue else []
        |> Seq.map (fun (u, s) -> SendTelegramMessage(u, formatSnapshot s) :> Command)

    [ newState; yield! messages ]
