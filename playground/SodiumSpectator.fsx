#r "nuget: SodiumFRP.FSharp, 5.0.6"

open Sodium.Frp

module Effects =
    let telegramMessageReceived = StreamSink.create<string * string> ()
    let telegramMessageSendRequested = StreamSink.create<string * string> ()

    type Callback = private { f: byte array -> unit }

    let createCallback f s =
        { f = fun g -> s |> StreamSink.send (f g) }

    let downloadRequested = StreamSink.create<{| url: string; callback: Callback |}> ()

module GlobalEvents =
    let newSubscriptionCreateRquested =
        StreamSink.create<{| user: string; url: string |}> ()

    let subscriptionCreated =
        StreamSink.create<{| user: string
                             url: string
                             mode: string |}>
            ()

let send stream arg () = stream |> StreamSink.send arg

let on e f =
    e |> Stream.map (fun arg state -> f arg state)

let makestore empty xs =
    xs |> Stream.mergeAll (fun x _ -> x) |> Stream.accum empty (fun f x -> f x)

module BotDomain =
    type private State = { subs: Map<string, obj list> }

    let private state =
        [ on GlobalEvents.newSubscriptionCreateRquested (fun arg state ->
              state.subs
              |> Map.tryFind arg.user
              |> Option.defaultValue []
              |> List.append [ arg.url ]
              |> fun x ->
                  { state with
                      subs = Map.add arg.user x state.subs }) ]
        |> makestore { subs = Map.empty }

    let _main =
        Effects.telegramMessageReceived
        |> Stream.snapshot state (fun (user, message) state ->
            match message.Split ' ' with
            | [| "/add"; url |] ->
                [ send GlobalEvents.newSubscriptionCreateRquested {| user = user; url = url |}
                  send Effects.telegramMessageSendRequested (user, "Sub added") ]
            | [| "/ls" |] ->
                let outMessage = state.subs |> Map.tryFind user |> Option.defaultValue [] |> string
                [ send Effects.telegramMessageSendRequested (user, outMessage) ]
            | _ -> [])

module WorkerDomain =
    let private downloadCompleted =
        StreamSink.create<{| user: string
                             url: string
                             data: byte array |}>
            ()

    let _main =
        [ downloadCompleted
          |> Stream.map (fun r ->
              [ send
                    GlobalEvents.subscriptionCreated
                    {| user = r.user
                       url = r.url
                       mode = "RSS" |} ])
          GlobalEvents.newSubscriptionCreateRquested
          |> Stream.map (fun e ->
              [ send
                    Effects.downloadRequested
                    {| url = e.url
                       callback =
                        Effects.createCallback
                            (fun b ->
                                {| url = e.url
                                   user = e.user
                                   data = b |})
                            downloadCompleted |} ]) ]
        |> Stream.mergeAll (fun x _ -> x)
