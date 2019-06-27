module Spectator.Worker.TelegramParser

open System
open TLSharp.Core
open TLSharp.Core.Network
open TeleSharp.TL
open TeleSharp.TL.Contacts
open TeleSharp.TL.Messages
module L = Spectator.Infrastructure.Log

module Configs =
    let appId = int <| Environment.GetEnvironmentVariable "SPECTATOR_TELEGRAM_APIID"
    let apiHash = Environment.GetEnvironmentVariable "SPECTATOR_TELEGRAM_APIHASH"
    let phone = Environment.GetEnvironmentVariable "SPECTATOR_USER_PHONENUMBER"
    let proxy = Environment.GetEnvironmentVariable "PROXY_HOST"
    let auth = Environment.GetEnvironmentVariable "PROXY_AUTH"

module ClientFactory =
    open Starksoft.Aspen.Proxy

    let make host port =
        let proxyClient =
            Socks5ProxyClient
                (Configs.proxy.Split(":").[0], Configs.proxy.Split(":").[1] |> int, Configs.auth.Split(":").[0],
                 Configs.auth.Split(":").[1])
        proxyClient.CreateConnection(host, port)

    let mkClient() =
        if isNull Configs.proxy then new TelegramClient(Configs.appId, Configs.apiHash)
        else new TelegramClient(Configs.appId, Configs.apiHash, handler = TcpClientConnectionHandler(make))

type TelegramState = { client : TelegramClient; hash : string }

let private sState = ref { client = null; hash = "" }

let reset =
    async {
        let client = ClientFactory.mkClient()
        do! client.ConnectAsync() |> Async.AwaitTask
        L.log "Telegram restarted" ()

        if not <| client.IsUserAuthorized() then
            let! hash = client.SendCodeRequestAsync(Configs.phone) |> Async.AwaitTask
            sState := { client = client; hash = hash }
            L.log (sprintf "Telegram required code (hash = %s)" hash) ()
        else
            sState := { client = client; hash = "" }
            L.log "Telegram authorized" ()
    }

let setCode code =
    async {
        L.log (sprintf "Telegram setCode called, code = %s" code) ()
        let state = !sState
        let! r = state.client.MakeAuthAsync(Configs.phone, state.hash, code) |> Async.AwaitTask |> Async.Catch
        L.log (sprintf "Telegram code applied, result = %O" r) ()
        // let! user = state.client.MakeAuthAsync(Configs.phone, state.hash, code) |> Async.AwaitTask
        // L.log (sprintf "Telegram code applied, user = %s" user.FirstName) ()
    }

let init () = Spectator.Core.sTelegramApi := Some (reset, setCode)

let private test chatName =
    async {
        let client = (!sState).client

        let r = TLRequestResolveUsername()
        r.Username <- chatName
        let! response = client.SendRequestAsync r |> Async.AwaitTask
        let channel = (response :> TLResolvedPeer).Chats.[0] :?> TLChannel
        printfn "Response = %O | id = %O" channel.Username channel.Id

        let i = TLInputPeerChannel()
        i.ChannelId <- channel.Id
        i.AccessHash <- channel.AccessHash.Value
        let! history = client.GetHistoryAsync(i, 0, 0, 50) |> Async.AwaitTask
        (history :?> TLChannelMessages).Messages
        |> Seq.choose (function :? TLMessage as x -> Some x | _ -> None)
        |> Seq.map (fun x -> x.Message)
        |> Seq.rev
        |> Seq.toList
        |> fun xs -> printfn "History [%i]:\n%A" xs.Length xs
    }

let isValid (uri : Uri) =
    async {
        if isNull (!sState).client then return false
        else
            let client = (!sState).client
            let r = TLRequestResolveUsername()
            r.Username <- uri.AbsolutePath
            let! response = client.SendRequestAsync r |> Async.AwaitTask
            return not (Seq.isEmpty <| (response :> TLResolvedPeer).Chats)
    }

let getNodes _ = async.Return []
