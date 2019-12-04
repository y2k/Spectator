module Bot

open MihaZupan
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

let private makeClient (env : EnvironmentConfig.Root) =
    match String.split (env.Telegram.Proxy ||| "") ':' with
    | host :: port :: _ ->
        let auth = String.split env.Telegram.Auth ':'
        let proxy = HttpToSocks5Proxy(host, int port, auth.[0], auth.[1])
        TelegramBotClient(env.Telegram.Token, proxy)
    | _ -> TelegramBotClient env.Telegram.Token

let sendToTelegramSingle env (user : string) message =
    let bot = makeClient env
    bot.SendTextMessageAsync(ChatId.op_Implicit user, message, parseMode = Enums.ParseMode.Html)
    |> (Async.AwaitTask >> Async.Catch)
    >>- function
        | Choice1Of2 _ -> SuccessResponse
        | Choice2Of2(:? AggregateException as ae) when (ae.InnerException :? Exceptions.ApiRequestException) ->
            BotBlockedResponse
        | Choice2Of2 e -> UnknownErrorResponse e

let repl env f = async {
    let bot = makeClient env
    let disposable =
        bot.OnUpdate
        |> Observable.map ^ fun args -> { text = args.Update.Message.Text ||| ""; user = string args.Update.Message.From.Id }
        |> Observable.subscribe ^ fun msg -> f msg >>= sendToTelegramSingle env msg.user |> (Async.Ignore >> Async.Start)

    bot.StartReceiving()

    do! Async.OnCancel(fun () -> disposable.Dispose(); bot.StopReceiving()) |> Async.Ignore
    do! Async.Sleep System.Int32.MaxValue }
