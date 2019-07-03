module Spectator.Worker.TelegramParser

open System
open TLSharp.Core
open TLSharp.Core.Network
open TeleSharp.TL
open TeleSharp.TL.Contacts
open TeleSharp.TL.Messages
open Spectator.Core
type L = Spectator.Infrastructure.Log

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
                (Configs.proxy.Split(':').[0], Configs.proxy.Split(':').[1] |> int, Configs.auth.Split(':').[0],
                 Configs.auth.Split(':').[1])
        proxyClient.CreateConnection(host, port)

    let mkClient() =
        if isNull Configs.proxy then new TelegramClient(Configs.appId, Configs.apiHash)
        else new TelegramClient(Configs.appId, Configs.apiHash, handler = TcpClientConnectionHandler(make))

type TelegramConnectorApiImpl() =

    let mutable client : TelegramClient = null
    let mutable hash : string = ""

    member __.debugGetClient = client

    interface TelegramConnectorApi with
        member __.resetClient = async {
            client <- ClientFactory.mkClient()
            do! client.ConnectAsync() |> Async.AwaitTask
            L.log "Telegram restarted"
            if client.IsUserAuthorized() then
                L.log "Telegram authorized"
            else
                let! h = client.SendCodeRequestAsync(Configs.phone) |> Async.AwaitTask
                hash <- h
                L.log ^ sprintf "Telegram required code (hash = %s)" hash
            return client.IsUserAuthorized() }
        member __.updateToken code = async {
            L.log ^ sprintf "Telegram setCode called, code = %s" code
            let! r = client.MakeAuthAsync(Configs.phone, hash, code) |> Async.AwaitTask |> Async.Catch
            L.log ^ sprintf "Telegram code applied, result = %O" r }
        member __.isValid uri = async {
            if isNull client then return false
            else
                let r = TLRequestResolveUsername()
                r.Username <- uri.AbsolutePath
                let! response = client.SendRequestAsync r |> Async.AwaitTask
                return not (Seq.isEmpty <| (response :> TLResolvedPeer).Chats) }
        member __.getNodes uri = async {
            let chatname = uri.Segments.[1]
            let r = TLRequestResolveUsername()
            r.Username <- chatname
            let! response = client.SendRequestAsync r |> Async.AwaitTask
            let channel = (response :> TLResolvedPeer).Chats.[0] :?> TLChannel
            L.log ^ sprintf "Response = %O | id = %O" channel.Username channel.Id
            let i = TLInputPeerChannel()
            i.ChannelId <- channel.Id
            i.AccessHash <- channel.AccessHash.Value
            let! history = client.GetHistoryAsync(i, 0, 0, 50) |> Async.AwaitTask
            return
                (history :?> TLChannelMessages).Messages
                |> Seq.choose ^ function | :? TLMessage as x -> Some x | _ -> None
                |> Seq.map ^
                    fun x ->
                        { subscriptionId = Guid.Empty
                          id = sprintf "telegram-%i" x.Id
                          title = x.Message
                          uri = Uri <| sprintf "https://t.me/%s/%i" chatname x.Id }
                |> Seq.rev
                |> Seq.toList }

let test chatName = async {
    IO.File.Delete "session.dat"
    let api = Spectator.Core.sTelegramApi
    let! authorized = api.resetClient
    if not authorized then
        printfn "Enter code:"
        let code = Console.ReadLine()
        do! api.updateToken code
    let! nodes = api.getNodes ^ Uri ^ sprintf "https://t.me/%s" chatName
    printfn "History [%i]:\n%A" nodes.Length nodes }
