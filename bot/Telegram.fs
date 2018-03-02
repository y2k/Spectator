module Bot

open System
open Telegram.Bot
open Telegram.Bot.Types
open Spectator.Core

type Message = { text: String; user: String }
type TelegramResponse = SuccessResponse | BotBlockedResponse | UnknownErrorResponse
let inline private (|?) (x: 'a) (def: 'a) = if isNull x then def else x

let private listerForMessages token =
    let bot = TelegramBotClient(token)
    let result = bot.OnUpdate 
                 |> Observable.map (fun args -> 
                     { text = args.Update.Message.Text |? ""; 
                       user = string args.Update.Message.From.Id })
    bot.StartReceiving()
    result

let private sendToTelegramSingle token (user: String) message =
    TelegramBotClient(token)
        .SendTextMessageAsync(ChatId.op_Implicit user, message, parseMode = Enums.ParseMode.Html) 
    |> (Async.AwaitTask >> Async.Catch)
    |> Async.map (
        function
        | Choice1Of2 _ -> SuccessResponse
        | Choice2Of2 (:? AggregateException as ae) when (ae.InnerException :? Exceptions.ApiRequestException) -> BotBlockedResponse
        | Choice2Of2 _ -> UnknownErrorResponse)

let repl token handler = 
    let handle token (message: Message) = 
        handler message
        |> Async.bind (sendToTelegramSingle token message.user)
        |> (Async.Ignore >> Async.Start)
    listerForMessages token |> Observable.add (handle token)