#r "nuget: SodiumFRP.FSharp, 5.0.6"
// #r "nuget: FsHtml, 0.2.0"

open System
open Sodium.Frp

module FsHtml =
    type Element =
        | Element of string * (string * obj) list * Element list
        | TextElement of string

    let div attrs children = Element("div", attrs, children)
    let input attrs children = Element("input", attrs, children)
    let span attrs children = Element("span", attrs, children)
    let attr (k: string) (v: obj) = k, v
    let str text = TextElement text

    let rec toString =
        function
        | TextElement x -> x
        | (Element(name, attrs, children)) ->
            let textAttrs =
                attrs
                |> List.choose (fun (k, v) ->
                    match v with
                    | :? String as s -> Some(sprintf "%s=\"%s\"" k s)
                    | _ -> None)
                |> function
                    | [] -> ""
                    | xs -> xs |> List.reduce (sprintf "%s %s")

            let textChildren =
                children
                |> List.map toString
                |> function
                    | [] -> ""
                    | xs -> xs |> List.reduce (sprintf "%s %s")

            sprintf "<%s %s>%s</%s>" name textAttrs textChildren name

module Effects =
    type Effect = Effect of (unit -> unit) list

    let send x stream =
        Effect [ fun _ -> Transaction.post (fun _ -> StreamSink.send x stream) ]

    let updateState s (state: _ Behavior) =
        let state = state :?> _ BehaviorSink
        Effect [ fun () -> Transaction.post (fun _ -> BehaviorSink.send s state) ]

    let updateC s (state: _ Cell) =
        let state = state :?> _ CellSink
        Effect [ fun () -> Transaction.post (fun _ -> CellSink.send s state) ]

    let multi (_effects: Effect list) : Effect =
        Effect(_effects |> List.collect (fun (Effect xs) -> xs))

module TelegramApi =
    let sendToTelegramRequested = StreamSink.create<string * string> ()
    let sTelegramBot = StreamSink.create<string * string> ()
    let downloadRequested = StreamSink.create<string * StreamSink<byte[]>> ()
    let uiUpdated = StreamSink.create<FsHtml.Element> ()

open Effects
open TelegramApi

module Sample1 =
    let _ignore () =
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

open FsHtml

type WeatherState =
    { text: string
      temps: string list
      textChanged: string StreamSink
      getWeatherClicked: unit StreamSink }

type AppState =
    { left: WeatherState
      right: WeatherState }

(* Weather App *)
let () =
    // let textChanged = StreamSink.create ()
    // let getWeatherClicked = StreamSink.create ()
    let downloadCompleted = StreamSink.create ()

    // let bText = CellSink.create ""
    // let bTemperature = CellSink.create ""

    // let result =
    //     [ getWeatherClicked
    //       |> Stream.snapshot bText (fun _ city ->
    //           send ($"https://weaher.api/?city={city}", downloadCompleted) downloadRequested)

    //       downloadCompleted
    //       |> Stream.snapshot2 bText bTemperature (fun resp text _ ->
    //           let temp = Text.Encoding.UTF8.GetString resp
    //           multi [ updateC temp bTemperature; send $"{(text, temp)}" uiUpdated ])

    //       textChanged
    //       |> Stream.snapshot2 bText bTemperature (fun newText _ temp ->
    //           multi [ updateC newText bText; send $"{(newText, temp)}" uiUpdated ])

    //       ]
    //     |> Stream.mergeAll (fun x _ -> x)

    // let result =
    //     [ textChanged
    //       |> Stream.snapshot bState (fun newText state ->
    //           multi
    //               [ updateC {| state with text = newText |} bState
    //                 send $"{(newText, state.temp)}" uiUpdated ])

    //       getWeatherClicked
    //       |> Stream.snapshot bState (fun _ state ->
    //           multi
    //               [ updateC {| state with temp = "..." |} bState
    //                 send ($"https://weaher.api/?city={state.text}", downloadCompleted) downloadRequested ])

    //       downloadCompleted
    //       |> Stream.snapshot bState (fun resp state ->
    //           let temp = Text.Encoding.UTF8.GetString resp

    //           multi
    //               [ updateC {| state with temp = temp |} bState
    //                 send $"{(state.text, temp)}" uiUpdated ])

    //       ]
    //     |> Stream.mergeAll (fun x _ -> x)

    let view (state: WeatherState) =
        div
            []
            [ input [ attr "value" state.text ] []
              div [] (List.map (fun temp -> span [] [ str temp ]) state.temps) ]

    // let bState =
    //     CellSink.create
    //         { text = ""
    //           temps = []
    //           textChanged = StreamSink.create ()
    //           getWeatherClicked = StreamSink.create ()
    //            }

    // let withUpdateUI state update eventStream =
    //     eventStream
    //     |> Stream.map  (fun eventArg  ->
    //         let newState, effs = (update state eventArg)

    //         multi
    //             [ yield updateC newState state
    //               yield send (view newState) uiUpdated
    //               yield! effs ])

    let weatherEffects (state: WeatherState) =
        [

          state.textChanged
          |> Stream.map (fun newText ->
              // let newState, effs = (update state eventArg)
              // let newState, effs = (fun state newText -> { state with text = newText }, [])
              let newState = { state with text = newText }

              multi
                  [ send (view newState) uiUpdated
                    // updateC newState state
                    ])

          //   state.textChanged
          //   |> withUpdateUI (fun state newText -> { state with text = newText }, [])

          //   state.getWeatherClicked
          //   |> withUpdateUI (fun state _ ->
          //       { state with text = "" },
          //       [ send ($"https://weaher.api/?city={state.text}", downloadCompleted) downloadRequested ])

          //   downloadCompleted
          //   |> withUpdateUI (fun state resp ->
          //       let temp = Text.Encoding.UTF8.GetString resp

          //       { state with
          //           temps = state.temps @ [ temp ] },
          //       [])

          ]
        |> Stream.mergeAll (fun x _ -> x)

    (* Parent *)

    let bParentState =
        CellSink.create
            {| left = (Cell.sample bState)
               right = (Cell.sample bState) |}

    let viewParent (state: {| left: _; right: _ |}) =
        div [] [ view state.left; view state.right ]

    let parentEffects (state: AppState) =
        [ weatherEffects state.left; weatherEffects state.right ]
        |> Stream.mergeAll (fun x _ -> x)

    (* Main *)

    use _ =
        downloadRequested
        |> Stream.listen (fun (url, cb) ->
            let city = url.Substring(6 + url.IndexOf "?city=")
            Transaction.post (fun _ -> StreamSink.send (Text.Encoding.UTF8.GetBytes $"{city}, 25C") cb))

    let ui =
        let rg = Text.RegularExpressions.Regex ">[\\s\r\n]+<"

        uiUpdated
        |> Stream.accum "" (fun ui _ -> ui |> toString |> (fun s -> rg.Replace(s, "><").Replace("\n", "")))

    let appState =
        { left =
            { text = ""
              temps = []
              textChanged = StreamSink.create ()
              getWeatherClicked = StreamSink.create () }
          right =
            { text = ""
              temps = []
              textChanged = StreamSink.create ()
              getWeatherClicked = StreamSink.create () } }

    use _ =
        parentEffects appState
        |> Stream.listen (fun (Effect fs) -> List.iter (fun f -> f ()) fs)

    let assert_ expected =
        let ui = Cell.sample ui

        if ui <> expected then
            failwithf "[ASSET] actual = '%s'" ui

    assert_ """"""

// StreamSink.send "Helsinki" textChanged
// assert_ """<div><input value="Helsinki"></input><div></div></div>"""

// StreamSink.send () getWeatherClicked
// assert_ """<div><input value=""></input><div><span>Helsinki, 25C</span></div></div>"""

// StreamSink.send "Oslo" textChanged
// assert_ """<div><input value="Oslo"></input><div><span>Helsinki, 25C</span></div></div>"""

// StreamSink.send () getWeatherClicked
// assert_ """<div><input value=""></input><div><span>Helsinki, 25C</span><span>Oslo, 25C</span></div></div>"""
