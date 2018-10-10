module Spectator.Worker.TelegramParser

open System
open TLSharp.Core
open TLSharp.Core.Network
open TeleSharp.TL
open TeleSharp.TL.Contacts
open TeleSharp.TL.Messages

module Configs =
    let appId = int <| Environment.GetEnvironmentVariable "TELEGRAM_APIID"
    let apiHash = Environment.GetEnvironmentVariable "TELEGRAM_APIHASH"
    let phone = Environment.GetEnvironmentVariable "USER_PHONENUMBER"
    let proxy = Environment.GetEnvironmentVariable "PROXY_HOST"
    let auth = Environment.GetEnvironmentVariable "PROXY_AUTH"

module ClientFactory =
    open Starksoft.Aspen.Proxy

    let make host port = 
        let proxyClient = Socks5ProxyClient(
                              Configs.proxy.Split(":").[0], Configs.proxy.Split(":").[1] |> int, 
                              Configs.auth.Split(":").[0],  Configs.auth.Split(":").[1])
        proxyClient.CreateConnection(host, port)

let private test chatName =
    async {
        use client = new TelegramClient(Configs.appId, 
                                        Configs.apiHash, 
                                        handler = TcpClientConnectionHandler(ClientFactory.make))
        do! client.ConnectAsync() |> Async.AwaitTask

        if not <| client.IsUserAuthorized() then
            let! hash = client.SendCodeRequestAsync(Configs.phone) |> Async.AwaitTask

            printfn "Enter code:"
            let code = Console.ReadLine()

            let! user = client.MakeAuthAsync(Configs.phone, hash, code) |> Async.AwaitTask
            printfn "User = %O" user.FirstName

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
