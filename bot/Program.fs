module Spectator.Bot.App

open System

module Domain =
    open Spectator.Core

    type Cmd =
        | GetUserSubscriptionsCmd
        | UnknownCmd
        | AddNewSubscriptionCmd of Uri * string option
        | DeleteSubscriptionCmd of Uri

    let parse (message : Bot.Message) =
        let isValidUri url = Uri.IsWellFormedUriString(url, UriKind.Absolute)
        match message.text with
        | Regex "/ls" [] -> GetUserSubscriptionsCmd
        | Regex "/add ([^ ]+) ([^ ]+)" [ url; filter ] when isValidUri url -> AddNewSubscriptionCmd (Uri url, Some filter)
        | Regex "/add ([^ ]+)" [ url ] when isValidUri url -> AddNewSubscriptionCmd (Uri url, None)
        | Regex "/rm ([^ ]+)" [ url ] -> DeleteSubscriptionCmd ^ Uri url
        | _ -> UnknownCmd

    let subListToMessageResponse (subscriptions : Subscription list) newSubscriptions userId =
        let subs = subscriptions |> List.filter ^ fun x -> x.userId = userId
        newSubscriptions
        |> List.filter ^ fun x -> x.userId = userId
        |> List.map ^ fun x -> sprintf "(Waiting) %O (%s)" x.uri x.filter
        |> List.append (subs |> List.map ^ fun x -> sprintf "%O (%s)" x.uri x.filter)
        |> List.fold (sprintf "%s\n- %s") "Your subscriptions: "

    let deleteSubs (subscriptions : Subscription list) newSubscriptions userId uri =
        newSubscriptions |> List.filter ^ fun x -> x.userId <> userId || x.uri <> uri
        , subscriptions |> List.filter ^ fun x -> x.userId <> userId || x.uri <> uri

    let add newSubscriptions user uri filter =
        let sub = { id = TypedId.wrap Guid.Empty; userId = user; uri = uri; filter = Option.defaultValue "" filter }
        sub :: newSubscriptions

module Updater =
    open Spectator.Core
    module P = Domain

    let handle message (db : CoEffectDb) =
        match P.parse message with
        | P.GetUserSubscriptionsCmd ->
            db, Domain.subListToMessageResponse db.subscriptions db.newSubscriptions message.user
        | P.DeleteSubscriptionCmd uri ->
            let (ns, ss) = Domain.deleteSubs db.subscriptions db.newSubscriptions message.user uri
            { db with subscriptions = ss; newSubscriptions = ns },
            "Your subscription deleted"
        | P.AddNewSubscriptionCmd (uri, filter) ->
            { db with newSubscriptions = Domain.add db.newSubscriptions message.user uri filter },
            "Your subscription created"
        | P.UnknownCmd -> 
            db, "/ls - Show your subscriptions\n/add [url] - Add new subscription\n/rm [url] - Add new subscription"

let start = Bot.repl (Updater.handle >> Spectator.Core.DependencyGraph.dbEff.run)
