namespace Spectator

open System
open Spectator.Core

module TelegramEventAdapter =
    let handleCommand sendToTelegram (cmd: Command) =
        match cmd with
        | :? SendTelegramMessage as SendTelegramMessage (user, msg) ->
            sendToTelegram user msg
            |> Async.Ignore
            |> Async.Start
        | _ -> ()

    let generateEvents readFromTelegram (dispatch: Event -> unit) =
        async {
            while true do
                let! (user: string, msg: string) = readFromTelegram
                dispatch (TelegramMessageReceived(user, msg))
        }

module TimerAdapter =
    let generateEvents (dispatch: Event -> unit) =
        async {
            let mutable i = 0L

            while true do
                dispatch (TimerTicked i)
                i <- i + 1L
                do! Async.Sleep 60_000
        }

module Https =
    let handleCommand downloadString dispatch (cmd: Command) =
        match cmd with
        | :? DownloadHttp as DownloadHttp (uri, callback) ->
            async {
                let! bytes = downloadString uri
                dispatch (callback bytes)
            }
            |> Async.Start
        | _ -> ()
