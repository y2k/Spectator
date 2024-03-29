namespace Spectator

open System
open Spectator.Core
open Y2k.EventBus

module TelegramEventAdapter =
    let handleCommand sendToTelegram (cmd: Command) =
        match cmd with
        | :? SendTelegramMessage as SendTelegramMessage (user, msg) ->
            sendToTelegram user msg |> Async.Ignore |> Async.Start
        | _ -> ()

    let generateEvents readFromTelegram (dispatch: Event -> unit) =
        async {
            while true do
                let! (user: string, msg: string) = readFromTelegram
                dispatch (TelegramMessageReceived(user, msg))
        }

module Https =
    open System.Net.Http

    let download (uri: Uri) =
        async {
            use client = new HttpClient()

            "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/605.1.15 (KHTML, like Gecko) Chrome/71.0.3578.98 Safari/605.1 Edge/19.17763"
            |> client.DefaultRequestHeaders.UserAgent.ParseAdd

            let! result = client.GetByteArrayAsync uri |> Async.AwaitTask |> Async.Catch

            return
                match result with
                | Choice1Of2 x -> Ok x
                | Choice2Of2 e -> Error e
        }

    let handleCommand downloadString dispatch (cmd: Command) =
        match cmd with
        | :? DownloadHttp as DownloadHttp (uris, callback) ->
            async {
                let! result =
                    uris
                    |> Seq.map (fun uri -> downloadString uri)
                    |> fun xs -> Async.Parallel(xs, 3)

                dispatch (callback (List.ofSeq result))
            }
            |> Async.Start
        | _ -> ()
