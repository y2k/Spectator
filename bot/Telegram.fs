module Bot

open System
open Telegram.Bot
open Spectator.Core
module RX = Observable

type Message = { text: string; user: string }
let inline private (|?) (x: 'a) (def: 'a) = if isNull x then def else x

let listerForMessages token =
    let bot = TelegramBotClient(token)
    let result = bot.OnUpdate 
                 |> RX.map (fun args -> 
                     let x = args.Update
                     { text = x.Message.Text |? ""; user = string x.Message.From.Id })
    bot.StartReceiving()
    result

type TelegramResponse = | SuccessResponse | BotBlockedResponse | UnknownErrorResponse
let sendToTelegramSingle token (user: string) message =
    try
        let bot = TelegramBotClient(token)
        bot.SendTextMessageAsync(user, message, parseMode = Types.Enums.ParseMode.Html).Result |> ignore
        SuccessResponse
    with
    | :? AggregateException as ae -> 
        match ae.InnerException with
        | :? Exceptions.ApiRequestException -> BotBlockedResponse
        | _                                 -> reraise()
    | _ -> UnknownErrorResponse