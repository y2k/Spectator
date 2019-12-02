module Spectator.Bot.App

module private Domain =
    open Spectator.Core
    open System
    type E = Spectator.Core.EnvironmentConfig.Root
    type R = System.Text.RegularExpressions.Regex

    type Eff =
        | TextEff of string
        | AsyncTextEff of string Async

    type private Cmd =
        | GetUserSubscriptionsCmd of UserId
        | UnknownCmd
        | AddNewSubscriptionCmd of UserId * Uri * string option
        | DeleteSubscriptionCmd of UserId * Uri
        | ResetTelegram
        | SetTelegramToken of string

    let private isValidFilter filter =
        String.isNullOrEmpty filter ||
            try R.IsMatch("", filter) |> ignore; true
            with :? ArgumentException -> false

    let private parse (message : Bot.Message) =
        match String.split message.text ' ' with
        | [ "/ls" ] -> GetUserSubscriptionsCmd message.user
        | "/add" :: url :: [ filter ] when isValidFilter filter ->
            AddNewSubscriptionCmd(message.user, Uri url, Some filter)
        | "/add" :: [ url ] -> AddNewSubscriptionCmd(message.user, Uri url, None)
        | "/rm" :: [ url ] -> DeleteSubscriptionCmd(message.user, Uri url)
        | [ "/telegram_reset" ] -> ResetTelegram
        | "/telegram_token" :: [ token ] -> SetTelegramToken token
        | _ -> UnknownCmd

    let subListToMessageResponse db userId =
        let subs = db.subscriptions |> List.filter ^ fun x -> x.userId = userId
        db.newSubscriptions
        |> List.filter ^ fun x -> x.userId = userId
        |> List.map ^ fun x -> sprintf "(Waiting) %O (%s)" x.uri x.filter
        |> List.append (subs |> List.map ^ fun x -> sprintf "%O (%s)" x.uri x.filter)
        |> List.fold (sprintf "%s\n- %s") "Your subscriptions: "

    let private deleteSubs db userId uri =
        { db with
            newSubscriptions = db.newSubscriptions |> List.filter ^ fun x -> x.userId <> userId || x.uri <> uri
            subscriptions = db.subscriptions |> List.filter ^ fun x -> x.userId <> userId || x.uri <> uri }
    
    let handle message (env : E) (db : CoEffectDb) =
        match parse message with
        | ResetTelegram when env.TelegramAdmin = message.user ->
            db, AsyncTextEff ^ async {
                let! authorized = sTelegramApi.resetClient
                return sprintf "Telegram recreated, authorized = %O" authorized }
        | SetTelegramToken token when env.TelegramAdmin = message.user ->
            db, AsyncTextEff ^ async {
                do! sTelegramApi.updateToken token
                return "Token accepted" }
        | GetUserSubscriptionsCmd userId ->
            db, TextEff ^ subListToMessageResponse db userId
        | DeleteSubscriptionCmd(userId, uri) ->
            deleteSubs db userId uri, TextEff "Your subscription deleted"
        | AddNewSubscriptionCmd(userId, uri, filter) ->
            let sub = { id = Guid.NewGuid(); userId = userId; uri = uri; filter = Option.defaultValue "" filter }
            { db with newSubscriptions = sub :: db.newSubscriptions },
            TextEff "Your subscription created"
        | _ -> db, TextEff "/ls - Show your subscriptions\n/add [url] - Add new subscription\n/rm [url] - Add new subscription"

open Spectator.Core
module C = Spectator.Infrastructure.MongoCofx

let start db env =
    Bot.repl env ^ fun msg ->
        async {
            match! C.runCfx db (Domain.handle msg env) with
            | Domain.TextEff t -> return t
            | Domain.AsyncTextEff at -> return! at }
