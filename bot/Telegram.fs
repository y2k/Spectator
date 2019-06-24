module Bot

open System
open Telegram.Bot
open Telegram.Bot.Types
open Spectator.Core
open MihaZupan

type Message =
    { text : string
      user : string }

type TelegramResponse =
    | SuccessResponse
    | BotBlockedResponse
    | UnknownErrorResponse of exn

let private makeClient() =
    let token = Environment.GetEnvironmentVariable "TELEGRAM_TOKEN"
    let hostPost = String.split (Environment.GetEnvironmentVariable "PROXY_HOST") ':'
    if List.isEmpty hostPost then
        TelegramBotClient(token)
    else
        let auth = String.split (Environment.GetEnvironmentVariable "PROXY_AUTH") ':'
        let proxy = HttpToSocks5Proxy(hostPost.[0], int hostPost.[1], auth.[0], auth.[1])
        TelegramBotClient(token, proxy)

let sendToTelegramSingle (user : string) message =
    let bot = makeClient()
    bot.SendTextMessageAsync(ChatId.op_Implicit user, message, parseMode = Enums.ParseMode.Html)
    |> (Async.AwaitTask >> Async.Catch)
    >>- function
    | Choice1Of2 _ -> SuccessResponse
    | Choice2Of2(:? AggregateException as ae) when (ae.InnerException :? Exceptions.ApiRequestException) ->
        BotBlockedResponse
    | Choice2Of2 e -> UnknownErrorResponse e

let repl f =
    async {
        let bot = makeClient()
        bot.OnUpdate
        |> Event.map ^ fun args -> { text = args.Update.Message.Text ||| ""; user = string args.Update.Message.From.Id }
        |> Event.add ^ fun msg -> f msg >>= sendToTelegramSingle msg.user |> (Async.Ignore >> Async.Start)

        bot.StartReceiving()

        do! Async.OnCancel(fun () -> bot.StopReceiving()) |> Async.Ignore
        do! Async.Sleep System.Int32.MaxValue
    }
