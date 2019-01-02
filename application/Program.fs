open System
open EasyNetQ
open Spectator.Core
open Spectator.Bot

module W = Spectator.Worker.App
module B = Spectator.Bot.App
module S = Spectator.Server.App

[<EntryPoint>]
let main _ =
    let inbox = S.start ()
    W.start inbox
    B.start inbox
    0
