﻿module Spectator.Bot.App

module private Domain =
    open System
    open Spectator.Core
    open MongoDB.Bson
    open MongoDB.Bson.Serialization

    type private Cmd =
        | GetUserSubscriptionsCmd of UserId
        | UnknownCmd
        | AddNewSubscriptionCmd of UserId * Uri

    let private parse (message : Bot.Message) =
        match String.split message.text ' ' with
        | "/ls" :: _ -> GetUserSubscriptionsCmd message.user
        | "/add" :: url :: _ -> AddNewSubscriptionCmd(message.user, Uri url)
        | _ -> UnknownCmd

    let private subListToMessageResponse (newSubs : NewSubscription list) (subs : Subscription list) =
        newSubs
        |> List.map (fun x -> sprintf "(Waiting) %O" x.uri)
        |> List.append (subs |> List.map (fun x -> string x.uri))
        |> List.fold (sprintf "%s\n- %s") "Your subscriptions: "

    let handle message (db : CoEffectDb) =
        match parse message with
        | GetUserSubscriptionsCmd userId ->
            db,
            subListToMessageResponse
                (db.newSubscriptions |> List.filter ^ fun x -> x.userId = userId)
                (db.subscriptions |> List.filter ^ fun x -> x.userId = userId)
        | AddNewSubscriptionCmd(userId, uri) ->
            let sub = { id = Guid.NewGuid(); userId = userId; uri = uri }
            { db with newSubscriptions = sub :: db.newSubscriptions },
            "Your subscription created"
        | UnknownCmd ->
            db, "/ls - show your subscriptions\n/add [url] - add new subscription"

open Spectator.Core
open Spectator.Infrastructure

let start db =
    Bot.repl ^ fun msg -> 
        dbContext db (Domain.handle msg)
