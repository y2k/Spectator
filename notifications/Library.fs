module Spectator.Notifications

open Spectator.Core

type State =
    { users : Map<Subscription TypedId, UserId>
      queue : (UserId * string) list
      initialized : bool }

module StoreDomain =
    let init = { users = Map.empty; queue = []; initialized = false }

    let private formatSnapshot snap =
        match snap.uri.ToString() with
        | Regex "https://t.me/.+" [] -> sprintf "%O" snap.uri
        | _ -> sprintf "%s\n\n<a href=\"%O\">[ OPEN ]</a>" snap.title snap.uri

    let update state = function
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

module InnerDomain =
    let main sendToTelegramSingle state =
        { state with queue = []; initialized = true }
        , state.queue |> List.map sendToTelegramSingle

module Domain =
    open System
    open InnerDomain
    open StoreDomain
    type R = Text.RegularExpressions.Regex

    type 'a Eff =
        | SendToTelegramSingle of UserId * string
        | Delay of TimeSpan * 'a

    type Msg =
        | EventReceived of Events
        | SendToTelegramEnd
        | StartMsg

    let init : State * Msg Eff list =
        StoreDomain.init, [ Delay (TimeSpan.Zero, StartMsg) ]

    let update e state =
        match e with
        | StartMsg ->
            let (state, effs) = main SendToTelegramSingle state
            state, effs |> List.append [ Delay (TimeSpan.FromSeconds 30., StartMsg) ]
        | SendToTelegramEnd -> state, []
        | EventReceived e -> update state e, []

let emptyState = StoreDomain.init

let restore state e = StoreDomain.update state e

let executeEffect sendToTelegramSingle eff =
    match eff with
    | Domain.Delay (time, m) ->
        Async.Sleep (int time.TotalMilliseconds) >>- always m
    | Domain.SendToTelegramSingle (u, msg) ->
        sendToTelegramSingle u msg
        >>- always Domain.SendToTelegramEnd
