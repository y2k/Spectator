open System
open EasyNetQ
module RX = Observable

module Domain =
    let handle (message: Bot.Message) =
        match message.text.Split(' ') |> Array.toList with
        | "ls"::_  -> "TODO: Your subscription"
        | "add"::_ -> "TODO: Add new subscription"
        | _        -> "TODO: Help"

[<EntryPoint>]
let main argv =
    Bot.listerForMessages argv.[0]
        |> RX.add (fun x -> 
            printfn "message: %O" x.text
            Bot.sendToTelegramSingle argv.[0] x.user (Domain.handle x) |> ignore)

    printfn "Listening for updates..."
    Threading.Thread.Sleep(-1);
    0