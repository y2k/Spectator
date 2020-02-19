module Spectator.Worker.TelegramParser

open System
open Spectator.Core
type L = Spectator.Infrastructure.Log

type private SnapsthoResponse = 
    { id : int 
      message : string }

let private getChatId = function
    | Regex "^https://t.me/([\\w\\d_]+)$" [ id ] -> Ok id
    | Regex "^([\\w\\d_]+)$" [ id ] -> Ok id
    | origin -> Error ^ sprintf "Can't find valid id from %s" origin
  
let private requestHistory (subUri : string) : Async<SnapsthoResponse[]> =
    async {
        let chatName = getChatId subUri |> Result.unwrap
        let url = sprintf "http://???/hisotory?chat=%s" chatName
        let client = new Net.Http.HttpClient()
        let! bytes = client.GetStringAsync(Uri url)
        let r = Newtonsoft.Json.JsonConvert.DeserializeObject<SnapsthoResponse[]>(bytes)
        return r
    }

let TelegramConnectorApi =
    { new HtmlProvider.IParse with
        member __.id = Guid.Parse "3B26457E-8AB7-41AD-8DEC-11AF891A3052" |> PluginId
        member __.isValid uri = 
            async {
                let! xs = requestHistory uri.AbsolutePath
                return not <| Seq.isEmpty xs
            }
        member __.getNodes uri = 
            async {
                match getChatId uri.AbsoluteUri with
                | Error _ -> return []
                | Ok chatName ->
                    let! xs = requestHistory uri.AbsolutePath
                    return
                        xs
                        |> Seq.map ^
                            fun x ->
                                { subscriptionId = SubscriptionId Guid.Empty
                                  id = sprintf "telegram-%i" x.id
                                  title = x.message
                                  uri = Uri <| sprintf "https://t.me/%s/%i" chatName x.id }
                        |> Seq.rev
                        |> Seq.toList 
            }
    }
