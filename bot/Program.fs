open System
open EasyNetQ
open Spectator.Core
open Spectator.Core.Utils

module RX = Observable

type TelegramCommand = Ls | Add of string | Unknown

module Bus =
    let request<'a, 'b when 'a : not struct and 'b : not struct> (bus: IBus) (msg: 'a): Async<'b> =
        bus.RequestAsync<'a, 'b>(msg)
        |> Async.AwaitTask

module Domain =
    let parse (message : string) = 
        match message.Split(' ') |> Array.toList with
        | "/ls" :: _ -> Ls
        | "/add" :: url :: _ -> Add url
        | _ -> Unknown

    let subListToMessageResponse (newSubs : NewSubscription list) (subs : Subscription list) = 
        newSubs
        |> List.map (fun x -> sprintf "(Waiting) %O" x.uri)
        |> List.append (subs |> List.map (fun x -> string x.uri))
        |> List.fold (sprintf "%s\n- %s") "Your subscriptions: "

    let mqResponseToTelegramReply = function 
        | UserSubscriptions(newSubs, subs) -> subListToMessageResponse newSubs subs
        | SubscriptionCreatedSuccessfull -> "Your subscription created"
        | NotCalledStub -> "/ls - show your subscriptions\n/add <url> - add new subscription"
        | _ -> "Unknow error"

module Services =
    let handleTelegramMessage (bus: IBus) (message: Bot.Message) = 
        match Domain.parse message.text with
        | Ls -> Bus.request bus <| GetUserSubscriptions message.user
        | Add url -> Bus.request bus <| AddNewSubscription (message.user, Uri url) 
        | Unknown -> Async.lift NotCalledStub
        |> Async.map Domain.mqResponseToTelegramReply

[<EntryPoint>]
let main _ =
    use bus = RabbitHutch.CreateBus("host=localhost")
    Environment.GetEnvironmentVariable "TELEGRAM_TOKEN"
    |> flip Bot.repl (Services.handleTelegramMessage bus)
    printfn "Listening for updates..."
    Threading.Thread.Sleep -1
    0