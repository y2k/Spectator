module Spectator.Logger

open Y2k.EventBus

let logEvent (event: Event) =
    printfn "== [EVENT] %s ==\n%A\n" (event.GetType().FullName) event
    []

let logCommand (cmd: Command) =
    printfn "== [CMD] %s ==\n%A\n" (cmd.GetType().FullName) cmd
