module Spectator.Bot.App

module private Domain =
    open MongoDB.Bson
    open MongoDB.Bson.Serialization
    open Spectator.Core
    open System

    type private Cmd =
        | GetUserSubscriptionsCmd of UserId
        | UnknownCmd
        | AddNewSubscriptionCmd of UserId * Uri
        | DeleteSubscriptionCmd of UserId * Uri

    let private parse (message : Bot.Message) =
        match String.split message.text ' ' with
        | "/ls" :: _ -> GetUserSubscriptionsCmd message.user
        | "/add" :: url :: _ -> AddNewSubscriptionCmd(message.user, Uri url)
        | "/rm" :: url :: _ -> DeleteSubscriptionCmd(message.user, Uri url)
        | _ -> UnknownCmd

    let private subListToMessageResponse (newSubs : NewSubscription list) (subs : Subscription list) =
        newSubs
        |> List.map (fun x -> sprintf "(Waiting) %O" x.uri)
        |> List.append (subs |> List.map (fun x -> string x.uri))
        |> List.fold (sprintf "%s\n- %s") "Your subscriptions: "

    let private deleteSubs db userId uri =
        { db with
            newSubscriptions = db.newSubscriptions |> List.filter ^ fun x -> x.userId <> userId || x.uri <> uri
            subscriptions = db.subscriptions |> List.filter ^ fun x -> x.userId <> userId || x.uri <> uri }

    let handle message (db : CoEffectDb) =
        match parse message with
        | GetUserSubscriptionsCmd userId ->
            db,
            subListToMessageResponse
                (db.newSubscriptions |> List.filter ^ fun x -> x.userId = userId)
                (db.subscriptions |> List.filter ^ fun x -> x.userId = userId)
        | DeleteSubscriptionCmd(userId, uri) ->
            deleteSubs db userId uri, "Your subscription deleted"
        | AddNewSubscriptionCmd(userId, uri) ->
            let sub = { id = Guid.NewGuid(); userId = userId; uri = uri }
            { db with newSubscriptions = sub :: db.newSubscriptions },
            "Your subscription created"
        | UnknownCmd ->
            db, "/ls - Show your subscriptions\n/add [url] - Add new subscription\n/rm [url] - Add new subscription"

open Spectator.Core
open Spectator.Infrastructure

let start db =
    Bot.repl ^ fun msg ->
        runCfx db (Domain.handle msg)
