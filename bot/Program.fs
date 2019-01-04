module Spectator.Bot.App

open System
open Spectator.Core

module R = Spectator.Server.App.Repository

type Cmd =
    | GetUserSubscriptionsCmd of UserId
    | UnknownCmd
    | AddNewSubscriptionCmd of UserId * Uri

module Domain =
    let parse (message : Bot.Message) =
        match String.split message.text ' ' with
        | "/ls" :: _ -> GetUserSubscriptionsCmd message.user
        | "/add" :: url :: _ -> AddNewSubscriptionCmd(message.user, Uri url)
        | _ -> UnknownCmd

    let subListToMessageResponse (newSubs : NewSubscription list) (subs : Subscription list) =
        newSubs
        |> List.map (fun x -> sprintf "(Waiting) %O" x.uri)
        |> List.append (subs |> List.map (fun x -> string x.uri))
        |> List.fold (sprintf "%s\n- %s") "Your subscriptions: "

module Services =
    let handleTelegramMessage' db message =
        async {
            match Domain.parse message with
            | GetUserSubscriptionsCmd userId -> let! mySubs = R.getSubscriptions db userId
                                                let! myNewSubs = R.getNewSubscriptions db userId
                                                return Domain.subListToMessageResponse myNewSubs mySubs
            | AddNewSubscriptionCmd(userId, uri) ->
                do! R.addNewSubscription db userId uri
                return "Your subscription created"
            | UnknownCmd -> return "/ls - show your subscriptions\n/add [url] - add new subscription"
        }

let start db = async { Bot.repl (Services.handleTelegramMessage' db) }

[<EntryPoint>]
let main _ = failwith "TODO"
