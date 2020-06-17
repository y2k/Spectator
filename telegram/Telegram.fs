module Spectator.Telegram

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

let private makeClient telegramToken =
    TelegramBotClient telegramToken

let sendToTelegramSingle telegramToken (user : string) message =
    let bot = makeClient telegramToken
    bot.SendTextMessageAsync(ChatId.op_Implicit user, message, parseMode = Enums.ParseMode.Html)
    |> (Async.AwaitTask >> Async.Catch)
    >>- function
        | Choice1Of2 _ -> SuccessResponse
        | Choice2Of2(:? AggregateException as ae) when (ae.InnerException :? Exceptions.ApiRequestException) ->
            BotBlockedResponse
        | Choice2Of2 e -> UnknownErrorResponse e

let readMessage token =
    let bot = makeClient token
    let offset = ref 0
    async {
        let! updates = bot.GetUpdatesAsync(offset = !offset, limit = 1, timeout = Int32.MaxValue) |> Async.AwaitTask
        let x = updates.[0]
        offset := x.Id + 1
        return string x.Message.From.Id, x.Message.Text
    }

let repl telegramToken f = 
    async {
        let bot = makeClient telegramToken
        let disposable =
            bot.OnUpdate
            |> Observable.map @@ fun args -> 
                string args.Update.Message.From.Id, args.Update.Message.Text ||| ""
            |> Observable.subscribe ^ fun msg -> 
                f msg
                >>- fun r -> printfn "Telegram ::\n>>>\n%O\n<<<\n%s" msg r; r
                >>= sendToTelegramSingle telegramToken (fst msg)
                |> (Async.Ignore >> Async.Start)

        bot.StartReceiving()

        do! Async.OnCancel(fun () -> disposable.Dispose(); bot.StopReceiving()) |> Async.Ignore
        do! Async.Sleep Int32.MaxValue
    }
