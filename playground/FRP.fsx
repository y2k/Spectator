#r "nuget: SodiumFRP.FSharp, 5.0.6"

open System
open Sodium.Frp

module Effects =
    type Effect = Effect of (unit -> unit) list

    let send x stream =
        Effect [ fun _ -> Transaction.post (fun _ -> StreamSink.send x stream) ]

    let updateState s (state: _ Behavior) =
        let state = state :?> _ BehaviorSink
        Effect [ fun () -> Transaction.post (fun _ -> BehaviorSink.send s state) ]

    let multi (_effects: Effect list) : Effect =
        Effect(_effects |> List.collect (fun (Effect xs) -> xs))

module TelegramApi =
    let sendToTelegramRequested = StreamSink.create<string * string> ()
    let sTelegramBot = StreamSink.create<string * string> ()
    let downloadRequested = StreamSink.create<string * StreamSink<byte[]>> ()

open Effects
open TelegramApi

let () =
    let downloadCompleted = StreamSink.create<byte[]> ()
    let bState = BehaviorSink.create {| isBusy = false; user = "" |} :> _ Behavior

    let result =
        [ downloadCompleted
          |> Stream.snapshotB bState (fun e state ->
              multi
                  [ updateState {| state with isBusy = false |} bState
                    send (state.user, $"Size = {Seq.length e}") sendToTelegramRequested ])
          sTelegramBot
          |> Stream.snapshotB bState (fun (user, text) state ->
              if not state.isBusy then
                  multi
                      [ updateState
                            {| state with
                                isBusy = true
                                user = user |}
                            bState
                        send (text, downloadCompleted) downloadRequested
                        send (user, "Url added to work") sendToTelegramRequested ]
              else
                  send (user, "Download in progress") sendToTelegramRequested) ]
        |> Stream.mergeAll (fun x _ -> x)

    (* MAIN *)

    use _ =
        downloadRequested
        |> Stream.listen (fun (url, cb) ->
            printfn "[DOWNLOAD] url = %s" url
            Transaction.post (fun _ -> StreamSink.send (Text.Encoding.UTF8.GetBytes url) cb))

    use _ =
        sendToTelegramRequested
        |> Stream.listen (fun (user, message) -> printfn "[OUTPUT]: [%s] %s" user message)

    use _ = result |> Stream.listen (fun (Effect fxs) -> List.iter (fun f -> f ()) fxs)

    let sendToTelegram message =
        printfn "[INPUT] %s" message
        StreamSink.send ("admin", message) sTelegramBot
        printfn "STATE: %A" (Behavior.sample bState)

    sendToTelegram "https://g.com/index.html"
