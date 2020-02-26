module Spectator.Bot.App

open System

module Parser =
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
        | Regex "/rm" [ url ] -> DeleteSubscriptionCmd ^ Uri url
        | _ -> UnknownCmd

module Domain =
    open Spectator.Core
    module P = Parser

    type Message = Message of string

    let subListToMessageResponse db userId =
        let subs = db.subscriptions |> List.filter ^ fun x -> x.userId = userId
        db.newSubscriptions
        |> List.filter ^ fun x -> x.userId = userId
        |> List.map ^ fun x -> sprintf "(Waiting) %O (%s)" x.uri x.filter
        |> List.append (subs |> List.map ^ fun x -> sprintf "%O (%s)" x.uri x.filter)
        |> List.fold (sprintf "%s\n- %s") "Your subscriptions: "

    let private deleteSubs db userId uri =
        { db with
            newSubscriptions = db.newSubscriptions |> List.filter ^ fun x -> x.userId <> userId || x.uri <> uri
            subscriptions = db.subscriptions |> List.filter ^ fun x -> x.userId <> userId || x.uri <> uri }

    let handle message (db : CoEffectDb) =
        match P.parse message with
        | P.GetUserSubscriptionsCmd ->
            db, Message ^ subListToMessageResponse db message.user
        | P.DeleteSubscriptionCmd uri ->
            deleteSubs db message.user uri, Message "Your subscription deleted"
        | P.AddNewSubscriptionCmd (uri, filter) ->
            let sub = { id = SubscriptionId ^ Guid.NewGuid(); userId = message.user; uri = uri; filter = Option.defaultValue "" filter }
            { db with newSubscriptions = sub :: db.newSubscriptions },
            Message "Your subscription created"
        | P.UnknownCmd -> db, Message "/ls - Show your subscriptions\n/add [url] - Add new subscription\n/rm [url] - Add new subscription"

open Spectator.Core

let start =
    Bot.repl ^ fun msg ->
        async {
            let! (Domain.Message t) = DependencyGraph.dbEff.run (Domain.handle msg)
            return t }
