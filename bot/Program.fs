module Spectator.Bot.App

open System
open EasyNetQ
open Spectator.Core

type Cmd =
    | GetUserSubscriptionsCmd of UserId
    | PingCmd
    | AddNewSubscriptionCmd of UserId * Uri

module Domain =
    let parse' (message : Bot.Message) =
        match String.split message.text ' ' with
        | "/ls" :: _ -> GetUserSubscriptionsCmd message.user
        | "/add" :: url :: _ -> AddNewSubscriptionCmd(message.user, Uri url)
        | _ -> PingCmd

    let subListToMessageResponse (newSubs : NewSubscription list) (subs : Subscription list) =
        newSubs
        |> List.map (fun x -> sprintf "(Waiting) %O" x.uri)
        |> List.append (subs |> List.map (fun x -> string x.uri))
        |> List.fold (sprintf "%s\n- %s") "Your subscriptions: "

module Services =
    let handleTelegramMessage' bus message =
        async {
            printfn "handleTelegramMessage | %O" message
            match Domain.parse' message with
            | GetUserSubscriptionsCmd userId ->
                return! Bus.reply bus (fun x -> GetUserSubscriptions'(userId, x))
                        >>- uncurry Domain.subListToMessageResponse
            | AddNewSubscriptionCmd(userId, uri) ->
                return! Bus.reply bus (fun x -> AddNewSubscription'(userId, uri, x))
                        >>- (fun _ -> "Your subscription created")
            | PingCmd -> return "/ls - show your subscriptions\n/add [url] - add new subscription"
        }

let start bus = Bot.repl (Services.handleTelegramMessage' bus)

[<EntryPoint>]
let main _ = failwith "TODO"
