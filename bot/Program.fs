open System
open EasyNetQ
open Spectator.Core
module RX = Observable

module Domain =
    type TelegramCommand = Ls | Add of string | Unknown

    let parse (message : string) = 
        match message.Split(' ') |> Array.toList with
        | "ls" :: _ -> Ls
        | "add" :: url :: _ -> Add url
        | _ -> Unknown

    let subListToMessageResponse (newSubs : NewSubscription list) (subs : Subscription list) = 
        newSubs
        |> List.map (fun x -> x.uri)
        |> List.append (subs |> List.map (fun x -> x.uri))
        |> List.fold (fun s x -> sprintf "%O\n- %O" s x) "Your subscriptions: "

    let responseToMessage (message: Bot.Message) = async {
        use bus = RabbitHutch.CreateBus("host=localhost")
        match parse message.text with
        | Ls -> 
            let! resp = bus.RequestAsync<Command, Responses>(GetUserSubscriptions message.user) |> Async.AwaitTask
            match resp with
            | UserSubscriptions(newSubs, subs) -> return subListToMessageResponse newSubs subs
            | _ -> return "Error"
        | Add url -> 
            let! resp = AddNewSubscription (message.user, Uri url) 
                        |> bus.RequestAsync<Command, Responses> |> Async.AwaitTask
            return "Your subscription created"
        | Unknown -> return "Commands: ls, add <url>"
    }

    let handle token (x: Bot.Message) = 
        async {
            printfn "message: %O" x.text
            let! res = responseToMessage x
            Bot.sendToTelegramSingle token x.user (res) |> ignore
        } |> Async.Start

[<EntryPoint>]
let main argv =
    Bot.listerForMessages argv.[0] |> RX.add (Domain.handle argv.[0])
    printfn "Listening for updates..."
    Threading.Thread.Sleep(-1);
    0