module Spectator.Notifications

open Spectator.Core

module Domain =
    type R = System.Text.RegularExpressions.Regex

    let private mkMessage (sub : Subscription) (snaps : Snapshot list) : string =
        let addLine prev snap = sprintf "%s\n- <a href=\"%O\">%s</a>" prev snap.uri snap.title
        let prefix = sprintf "Update for <code>%O</code> (%i):" sub.uri (List.length snaps)
        List.fold addLine prefix snaps

    let private matchContent content optRegex =
        if String.isNullOrEmpty optRegex 
            then true
            else R.IsMatch(content, optRegex)

    let private getUpdates (subscriptions : Subscription list) snaps =
        let findSnapsForSub (sub : Subscription) =
            snaps
            |> List.filter ^ fun x ->
                x.subscriptionId = sub.id && matchContent x.title sub.filter

        subscriptions
        |> List.map ^ fun sub -> sub, findSnapsForSub sub
        |> List.filter ^ fun (_, snaps) -> List.isNotEmpty snaps

    type Eff = SendToTelegramSingle of UserId * string
    type State = { users : Map<Subscription TypedId, UserId> }

    let init = { users = Map.empty }

    let update e state =
        match e with
        | SnapshotCreated snap ->
            match Map.tryFind snap.subscriptionId state.users with
            | Some userId ->
                let msg = sprintf "- <a href=\"%O\">%s</a>" snap.uri snap.title
                state, [ SendToTelegramSingle (userId, msg) ]
            | None -> state, []
        | SubscriptionCreated sub ->
            { state with users = Map.add sub.id sub.userId state.users }, []
        | SubscriptionRemoved (subId, _) ->
            { state with 
                users = state.users 
                        |> Map.filter @@ fun k _ -> not @@ List.contains k subId }
            , []
        | _ -> state, []

let emptyState = Domain.init

let restore state e =
    Domain.update e state |> fst

let main initState readEvent sendToTelegramSingle =
    let executeEff eff =
        match eff with
        | Domain.SendToTelegramSingle (u, msg) -> sendToTelegramSingle u msg

    let state = ref initState
    let rec loopReadEvents () =
        async {
            let! (e : Events) = readEvent
            let (s2, effs) = Domain.update e !state
            state := s2
            do! effs |> List.map executeEff |> Async.Parallel |> Async.Ignore
            do! loopReadEvents ()
        }
    loopReadEvents ()
