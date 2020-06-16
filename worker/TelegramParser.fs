module Spectator.Worker.TelegramParser

open System
open System.Net.Http
open Spectator.Core
type L = Log
open Newtonsoft.Json

type private SnapshotResponse = { title : string; author : string; id : string }

let Parser restTelegramPassword restTelegramBaseUrl =
    let toChatName (_ : Uri) : string = failwith "???"
    let getChatId (uri : Uri) = 
        uri.AbsoluteUri
        |> function
           | Regex "^https://t.me/([\\w\\d_]+)$" [ id ] -> Ok id
           | origin -> Error ^ sprintf "Can't find valid id from %s" origin

    { new HtmlProvider.IParse with
        member __.id = Guid.Parse "3B26457E-8AB7-41AD-8DEC-11AF891A3052"
        member __.isValid uri =
            match getChatId uri with Ok _ -> true | Error _ -> false
            |> async.Return
        member __.getNodes uri =
            async {
                let chat = toChatName uri
                let client = new HttpClient()
                let auth =
                    sprintf "_:%s" restTelegramPassword
                    |> Text.Encoding.UTF8.GetBytes
                    |> Convert.ToBase64String
                client.DefaultRequestHeaders.Authorization <- Headers.AuthenticationHeaderValue("Basic", auth)
                let! json = 
                    sprintf "%s/history?chat=%s" restTelegramBaseUrl (toChatName uri)
                    |> client.GetStringAsync
                return
                    JsonConvert.DeserializeObject<SnapshotResponse[]> json
                    |> Seq.toList
                    |> List.map ^ fun x ->
                          { subscriptionId = TypedId.empty ()
                            created = failwith "???"
                            id = TypedId.empty ()
                            title = x.title
                            uri = Uri ^ sprintf "https://t.me/%s/%s" chat x.id } } }
