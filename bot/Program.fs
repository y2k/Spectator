open System
open EasyNetQ
open Spectator.Core
module RX = Observable

let flip f a b = f b a

module Domain =
    type TelegramCommand = Ls | Add of string | Unknown

    let parse (message : string) = 
        match message.Split(' ') |> Array.toList with
        | "ls" :: _ -> Ls
        | "add" :: url :: _ -> Add url
        | _ -> Unknown

    let subListToMessageResponse (newSubs : NewSubscription list) (subs : Subscription list) = 
        newSubs
        |> List.map (fun x -> sprintf "(Waiting) %O" x.uri)
        |> List.append (subs |> List.map (fun x -> string x.uri))
        |> List.fold (fun s x -> sprintf "%s\n- %s" s x) "Your subscriptions: "

    let mqResponseToTelegramReply = function 
        | UserSubscriptions(newSubs, subs) -> subListToMessageResponse newSubs subs
        | SubscriptionCreatedSuccessfull -> "Your subscription created"
        | NotCalledStub -> "Show help" // TODO:
        | _ -> "Unknow error"

    let handleTelegramMessage (message: Bot.Message) = async {
        use bus = RabbitHutch.CreateBus("host=localhost")
        let! resp = async {
            match parse message.text with
            | Ls -> 
                return! bus.RequestAsync<Command, Responses>(GetUserSubscriptions message.user) 
                        |> Async.AwaitTask
            | Add url -> 
                return! AddNewSubscription (message.user, Uri url) 
                        |> bus.RequestAsync<Command, Responses> 
                        |> Async.AwaitTask
            | Unknown -> return NotCalledStub
        }
        return mqResponseToTelegramReply resp
    }

[<EntryPoint>]
let main _ =
    Environment.GetEnvironmentVariable "TELEGRAM_TOKEN"
    |> flip Bot.repl Domain.handleTelegramMessage
    printfn "Listening for updates..."
    Threading.Thread.Sleep -1
    0