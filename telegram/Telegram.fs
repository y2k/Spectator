module Bot

open Spectator.Core
open System
open Telegram.Bot
open Telegram.Bot.Types

type Message =
    { text : string
      user : string }

type TelegramResponse =
    | SuccessResponse
    | BotBlockedResponse
    | UnknownErrorResponse of exn

let private makeClient () =
    TelegramBotClient DependencyGraph.config.telegramToken

let sendToTelegramSingle (user : string) message =
    let bot = makeClient ()
    bot.SendTextMessageAsync(ChatId.op_Implicit user, message, parseMode = Enums.ParseMode.Html)
    |> (Async.AwaitTask >> Async.Catch)
    >>- function
        | Choice1Of2 _ -> SuccessResponse
        | Choice2Of2(:? AggregateException as ae) when (ae.InnerException :? Exceptions.ApiRequestException) ->
            BotBlockedResponse
        | Choice2Of2 e -> UnknownErrorResponse e

let repl f = async {
    let bot = makeClient ()
    let disposable =
        bot.OnUpdate
        |> Observable.map ^ fun args -> { text = args.Update.Message.Text ||| ""; user = string args.Update.Message.From.Id }
        |> Observable.subscribe ^ fun msg -> 
            f msg
            >>- fun r -> printfn "Telegram ::\n>>>\n%O\n<<<\n%s" msg r; r
            >>= sendToTelegramSingle msg.user 
            |> (Async.Ignore >> Async.Start)

    bot.StartReceiving()

    do! Async.OnCancel(fun () -> disposable.Dispose(); bot.StopReceiving()) |> Async.Ignore
    do! Async.Sleep System.Int32.MaxValue }
