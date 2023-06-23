#r "nuget: SodiumFRP.FSharp, 5.0.6"
#r "nuget: FsHtml, 0.2.0"

open Sodium.Frp

module EffectApi =
    type DownloadResponse = DownloadResponse of string
    type DownloadRequest = DownloadRequest of string * StreamSink<DownloadResponse>

    let downloadRequested = StreamSink.create<DownloadRequest> ()
    let updateUi = StreamSink.create<string> ()

module WeatherApp =
    open EffectApi

    let textChanged = StreamSink.create<string> ()
    let getWeatherClicked = StreamSink.create<Unit> ()
    let private downloadCompleted = StreamSink.create<DownloadResponse> ()

    let outStream =
        [ textChanged |> Stream.map (fun _x -> ())
          getWeatherClicked |> Stream.map (fun _x -> ())
          downloadCompleted |> Stream.map (fun _x -> ()) ]
        |> Stream.mergeAll (fun x _ -> x)

    (*

        (def out-stream
             (->
              []
              (stream/merge-all (fn [x _] x))))

         *)

    // type State = { text: string; weather: string }

    // let _a =
    //     // getWeatherClicked
    //     textChanged
    //     |> Stream.

    let private bStore = Stream.accum "" (fun x _ -> x) textChanged |> Cell.asBehavior
    // let private sStore = Stream.collect "" (fun eArg _ -> struct (eArg, eArg)) textChanged

    let bUi =
        Behavior.lift2
            (fun a b -> a, b)
            (bStore,
             Stream.accum "" (fun (DownloadResponse x) _ -> x) downloadCompleted
             |> Cell.asBehavior)
        |> Behavior.map string

    let main =
        [ Stream.snapshotB
              bStore
              (fun _ city -> DownloadRequest($"https://g.com/api?weather=${city}", downloadCompleted))
              getWeatherClicked
          |> Stream.map (fun r () -> StreamSink.send r downloadRequested)
          textChanged
          |> Stream.snapshotB bUi (fun _ x -> x)
          |> Stream.map (fun x () -> StreamSink.send (Behavior.sample bUi) updateUi) ]
        |> Stream.mergeAll (fun a _ -> a)

let () =
    EffectApi.downloadRequested
    |> Stream.listen (fun (EffectApi.DownloadRequest(url, cb)) ->
        printfn "LOG:FX:downloadRequested:%O" url
        StreamSink.send (EffectApi.DownloadResponse "weather") cb)
    |> ignore

    EffectApi.updateUi |> Stream.listen (printfn "LOG:FX:updateUi:%O") |> ignore

    WeatherApp.main
    |> Stream.listen (fun fxs ->
        printfn "LOG:FX:main:%A" fxs
        Transaction.post (fun _ -> fxs ()))

    |> ignore

    StreamSink.send "hello-world" WeatherApp.textChanged
