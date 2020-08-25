module Spectator.Notifications

open Spectator.Core

module Domain =
    open System
    type R = Text.RegularExpressions.Regex

    let formatSnapshot snap =
        match snap.uri.ToString() with
        | Regex "https://t.me/.+" [] -> sprintf "%O" snap.uri
        | _ -> sprintf "%s\n\n<a href=\"%O\">[ OPEN ]</a>" snap.title snap.uri

    type 'a Eff =
        | SendToTelegramSingle of UserId * string
        | Delay of TimeSpan * 'a

    type State = { users : Map<Subscription TypedId, UserId>; queue : (UserId * string) list; initialized : bool }

    type Msg =
        | EventReceived of Events
        | SendToTelegramEnd
        | StartMsg

    let init : State * Msg Eff list =
        { users = Map.empty; queue = []; initialized = false }
        , [ Delay (TimeSpan.Zero, StartMsg) ]

    let updateStore state = function
        | SnapshotCreated snap ->
            if state.initialized then
                match Map.tryFind snap.subscriptionId state.users with
                | Some userId -> { state with queue = (userId, formatSnapshot snap) :: state.queue }
                | None -> state
            else state
        | SubscriptionCreated sub ->
            { state with users = Map.add sub.id sub.userId state.users }
        | SubscriptionRemoved (subId, _) ->
            { state with
                users = state.users
                        |> Map.filter @@ fun k _ -> not @@ List.contains k subId }
        | _ -> state

    let private main state =
        { state with queue = []; initialized = true }
        , state.queue |> List.map SendToTelegramSingle

    let update e state =
        match e with
        | StartMsg ->
            let (state, effs) = main state
            state, effs |> List.append [ Delay (TimeSpan.FromSeconds 30., StartMsg) ]
        | SendToTelegramEnd -> state, []
        | EventReceived e -> updateStore state e, []

let emptyState = fst Domain.init
let restore state e = Domain.updateStore state e

let executeEffect sendToTelegramSingle eff =
    match eff with
    | Domain.Delay (time, m) ->
        Async.Sleep (int time.TotalMilliseconds) >>- always m
    | Domain.SendToTelegramSingle (u, msg) ->
        sendToTelegramSingle u msg
        >>- always Domain.SendToTelegramEnd
