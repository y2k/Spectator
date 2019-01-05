module Bot

open System
open Telegram.Bot
open Telegram.Bot.Types
open Spectator.Core
open MihaZupan

type Message =
    { text : String
      user : String }

type TelegramResponse =
    | SuccessResponse
    | BotBlockedResponse
    | UnknownErrorResponse

let inline private (|?) (x : 'a) (def : 'a) =
    if isNull x then def
    else x

let private makeClient() : TelegramBotClient =
    let token = Environment.GetEnvironmentVariable "TELEGRAM_TOKEN"
#if DEBUG
    let hostPost = String.split (Environment.GetEnvironmentVariable "PROXY_HOST") ':'
    let auth = String.split (Environment.GetEnvironmentVariable "PROXY_AUTH") ':'
    let proxy = HttpToSocks5Proxy(hostPost.[0], int hostPost.[1], auth.[0], auth.[1])
    TelegramBotClient(token, proxy)
#else
    TelegramBotClient(token)
#endif

let private listerForMessages() =
    let bot = makeClient()

    let result =
        bot.OnUpdate
        |> Observable.map (fun args ->
               { text = args.Update.Message.Text |? ""
                 user = string args.Update.Message.From.Id })
    bot.StartReceiving()
    result

let private sendToTelegramSingle (user : String) message =
    let bot = makeClient()
    bot.SendTextMessageAsync(ChatId.op_Implicit user, message, parseMode = Enums.ParseMode.Html)
    |> (Async.AwaitTask >> Async.Catch)
    >>- function
    | Choice1Of2 _ -> SuccessResponse
    | Choice2Of2(:? AggregateException as ae) when (ae.InnerException :? Exceptions.ApiRequestException) ->
        BotBlockedResponse
    | Choice2Of2 _ -> UnknownErrorResponse

let repl handler =
    let handle (message : Message) =
        handler message
        >>= sendToTelegramSingle message.user
        |> (Async.Ignore >> Async.Start)
    listerForMessages() |> Observable.add handle
    printfn "Listening for updates..."
    Threading.Thread.Sleep -1
