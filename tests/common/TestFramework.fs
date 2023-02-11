module TestFramework

open System
open System.Threading.Channels
open Swensen.Unquote
open Spectator
open Spectator.Core
open Spectator.Store

let rec private flaky count f =
    async {
        try
            do! f
        with e ->
            if count > 0 then
                do! Async.Sleep 1_000
                do! flaky (count - 1) f
            else
                raise e
    }

let private assertBot repeat (input: Channel<string>) (output: Channel<string>) msg expected =
    let mutable prev = "<not set>"

    let read () =
        let (success, v) = output.Reader.TryRead()

        if success then
            prev <- v
            v
        else
            prev

    if not repeat && not <| String.IsNullOrEmpty msg then
        input.Writer.WriteAsync msg |> ignore

    flaky
        10
        (async {
            if repeat && not <| String.IsNullOrEmpty msg then
                input.Writer.WriteAsync msg |> ignore

            let actual = read ()
            // printfn "\nLOG :: %O\n" actual
            test <@ expected = actual @>
        })

let mkDownloadString stage (url: Uri) =
    IO.Path.Combine(
        IO.Directory.GetCurrentDirectory(),
        sprintf "../../../../tests/examples/%i/" stage,
        Uri.EscapeDataString(string url)
    )
    |> IO.File.ReadAllBytes
    |> Ok
    |> async.Return

let private mkApplication (output: string Channel) (input: string Channel) db (downloadString: _ ref) (time: TimeSpan) =
    let persCache = Persistent.make db

    Application.runApplication persCache
    |> Router.addCommand (Https.handleCommand (fun url -> downloadString.Value url))
    |> Router.addCommand_ (
        TelegramEventAdapter.handleCommand (fun userId message ->
            (output.Writer.WriteAsync message).AsTask() |> Async.AwaitTask)
    )
    |> Router.addEventGenerator (
        TelegramEventAdapter.generateEvents (
            async {
                let! m = input.Reader.ReadAsync()
                return "0", m
            }
        )
    )
    |> Router.addCommand (fun dispatch cmd ->
        match cmd with
        | :? DispatchWithTimeout as DispatchWithTimeout (_, e) ->
            async {
                do! Async.Sleep time
                dispatch e
            }
            |> Async.Start
        | :? DispatchWithInterval as DispatchWithInterval (_, e) ->
            async {
                while true do
                    do! Async.Sleep time
                    dispatch e
            }
            |> Async.Start
        | _ -> ())
    |> Router.start (Persistent.RestoreStateEvent persCache)

type TestState =
    private
        { output: Channel<string>
          input: Channel<string>
          downloadString: (Uri -> Async<Result<byte array, exn>>) ref
          dbPath: string }

let executeCommand (state: TestState) cmd expected =
    assertBot true state.input state.output cmd expected |> Async.RunSynchronously

let executeCommandOnce (state: TestState) cmd expected =
    assertBot false state.input state.output cmd expected |> Async.RunSynchronously

let waitForMessage (state: TestState) expected =
    assertBot false state.input state.output "" expected |> Async.RunSynchronously

let setDownloadStage (state: TestState) stage =
    state.downloadString.Value <- mkDownloadString stage

let startApplication () : TestState =
    let state =
        { input = Channel.CreateUnbounded<string>()
          output = Channel.CreateUnbounded<string>()
          downloadString = ref (mkDownloadString 0)
          dbPath = IO.Path.GetTempFileName() }

    mkApplication state.output state.input state.dbPath state.downloadString (TimeSpan.FromSeconds 2.0)
    |> Async.Start

    state

let resetApplication (oldState: TestState) : TestState =
    let state =
        { input = Channel.CreateUnbounded<string>()
          output = Channel.CreateUnbounded<string>()
          downloadString = ref (mkDownloadString 0)
          dbPath = oldState.dbPath }

    mkApplication state.output state.input state.dbPath state.downloadString (TimeSpan.FromSeconds 2.0)
    |> Async.Start

    state
