module Spectator.Notifications

open Spectator.Core

module Domain =
    let formatSnapshot snap =
        match snap.uri.ToString() with
        | Regex "https://t.me/.+" [] -> sprintf "%O" snap.uri
        | _ -> sprintf "%s\n\n<a href=\"%O\">[ OPEN ]</a>" snap.title snap.uri

    type R = System.Text.RegularExpressions.Regex
    type Eff = SendToTelegramSingle of UserId * string
    type State = { users : Map<Subscription TypedId, UserId> }

    let init = { users = Map.empty }

    type Msg =
        | EventReceived of Events
        | SendToTelegramEnd

    let private updateStore state =
        function
        | SnapshotCreated snap ->
            match Map.tryFind snap.subscriptionId state.users with
            | Some userId -> state, [ SendToTelegramSingle (userId, formatSnapshot snap) ]
            | None -> state, []
        | SubscriptionCreated sub ->
            { state with users = Map.add sub.id sub.userId state.users }, []
        | SubscriptionRemoved (subId, _) ->
            { state with
                users = state.users
                        |> Map.filter @@ fun k _ -> not @@ List.contains k subId }
            , []
        | _ -> state, []

    let update e state =
        match e with
        | SendToTelegramEnd -> state, []
        | EventReceived e -> updateStore state e

let emptyState = Domain.init
let restore state e = Domain.update (Domain.EventReceived e) state |> fst

let executeEffect sendToTelegramSingle eff =
    match eff with
    | Domain.SendToTelegramSingle (u, msg) ->
        sendToTelegramSingle u msg
        >>- always Domain.SendToTelegramEnd

let main initState readEvent sendToTelegramSingle =
    let executeEffect = executeEffect sendToTelegramSingle
    Tea.start initState [] Domain.update Domain.EventReceived executeEffect readEvent
