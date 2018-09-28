open System
open EasyNetQ
open Spectator.Core

module Bus =
    let request (bus: IBus) command =
        bus.RequestAsync<Command, Responses>(command)
        |> Async.AwaitTask

module Domain =
    let parse (message : Bot.Message) = 
        match String.split message.text ' ' with
        | "/ls"  :: _        -> GetUserSubscriptions message.user
        | "/add" :: url :: _ -> AddNewSubscription (message.user, Uri url) 
        | _                  -> Ping

    let private subListToMessageResponse (newSubs : NewSubscription list) (subs : Subscription list) = 
        newSubs
        |> List.map (fun x -> sprintf "(Waiting) %O" x.uri)
        |> List.append (subs |> List.map (fun x -> string x.uri))
        |> List.fold (sprintf "%s\n- %s") "Your subscriptions: "

    let responeToMessage = function 
        | UserSubscriptions(newSubs, subs) -> subListToMessageResponse newSubs subs
        | SubscriptionCreatedSuccessfull -> "Your subscription created"
        | EmptyResponse -> "/ls - show your subscriptions\n/add [url] - add new subscription"
        | _ -> "Unknow error"

module Services =
    let handleTelegramMessage bus message = 
        printfn "handleTelegramMessage | %O" message
        Domain.parse message
        |> Bus.request bus
        >>- Domain.responeToMessage

[<EntryPoint>]
let main _ =
    use bus = RabbitHutch.CreateBus("host=localhost")
    Bot.repl (Services.handleTelegramMessage bus)
    0