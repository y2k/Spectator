module Spectator.Worker.TelegramParser

open System
open System.Net.Http
open Spectator.Core

type L = Log
open Newtonsoft.Json

type SnapshotResponse =
    { message: string
      author: string
      id: string
      created: DateTime }

let private getChatId (uri: Uri) =
    uri.AbsoluteUri
    |> function
    | Regex "^https://t.me/([\\w\\d_]+)$" [ id ] -> Ok id
    | origin ->
        Error
        ^ sprintf "Can't find valid id from %s" origin

let getNodes restTelegramPassword restTelegramBaseUrl uri =
    async {
        let chat =
            getChatId uri
            |> function
            | Ok id -> id
            | Error e -> failwith e

        let client = new HttpClient()

        let auth =
            sprintf "_:%s" restTelegramPassword
            |> Text.Encoding.UTF8.GetBytes
            |> Convert.ToBase64String

        client.DefaultRequestHeaders.Authorization <- Headers.AuthenticationHeaderValue("Basic", auth)

        let! json =
            sprintf "%s%s" restTelegramBaseUrl (Uri.EscapeDataString chat)
            |> client.GetStringAsync

        return
            JsonConvert.DeserializeObject<SnapshotResponse []> json
            |> Seq.toList
            |> List.map
               ^ fun x ->
                   { subscriptionId = TypedId.empty ()
                     created = x.created
                     id = TypedId.empty ()
                     title = x.message
                     uri = Uri <| sprintf "https://t.me/%s/%s" chat x.id }
    }

let create restTelegramPassword restTelegramBaseUrl =
    {| id = Guid.Parse "3B26457E-8AB7-41AD-8DEC-11AF891A3052"
       isValid =
           fun uri ->
               match getChatId uri with
               | Ok _ -> true
               | Error _ -> false
               |> async.Return
       getNodes = fun uri -> getNodes restTelegramPassword restTelegramBaseUrl uri |}
