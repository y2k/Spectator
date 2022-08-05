module Spectator.Logger

open Spectator.Core

let logEvent (event: Event) =
    printfn "== [EVENT] %s ==\n%A\n" (event.GetType().FullName) event
    []

let logCommand (cmd: Command) =
    printfn "== [CMD] %s ==\n%A\n" (cmd.GetType().FullName) cmd
