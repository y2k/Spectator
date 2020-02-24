module Spectator.Worker.TelegramParser

open System
open System.Net.Http
open Spectator.Core
type L = Spectator.Infrastructure.Log
open Newtonsoft.Json

type private SnapshotResponse = { title : string; author : string; id : string }

let TelegramConnectorApiImpl =
    let toChatName (_ : Uri) : string = failwith "???"
    let getChatId (uri : Uri) = 
        uri.AbsoluteUri
        |> function
           | Regex "^https://t.me/([\\w\\d_]+)$" [ id ] -> Ok id
           | origin -> Error ^ sprintf "Can't find valid id from %s" origin

    { new Spectator.Worker.HtmlProvider.IParse with
        member __.id = PluginId ^ Guid.Parse "3B26457E-8AB7-41AD-8DEC-11AF891A3052"
        member __.isValid uri =
            match getChatId uri with Ok _ -> true | Error _ -> false
            |> async.Return
        member __.getNodes uri =
            async {
                let chat = toChatName uri
                let client = new HttpClient()
                let auth =
                    sprintf "_:%s" DependencyGraph.config.restTelegramPassword
                    |> Text.Encoding.UTF8.GetBytes
                    |> Convert.ToBase64String
                client.DefaultRequestHeaders.Authorization <- Headers.AuthenticationHeaderValue("Basic", auth)
                let! json = 
                    sprintf "%s/history?chat=%s" DependencyGraph.config.restTelegramBaseUrl (toChatName uri)
                    |> client.GetStringAsync
                return
                    JsonConvert.DeserializeObject<SnapshotResponse[]> json
                    |> Seq.toList
                    |> List.map ^ fun x ->
                          { subscriptionId = SubscriptionId Guid.Empty
                            id = x.id
                            title = x.title
                            uri = Uri ^ sprintf "https://t.me/%s/%s" chat x.id } } }
