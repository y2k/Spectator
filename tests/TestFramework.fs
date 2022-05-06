module TestFramework

open System
open System.Threading.Channels
open Swensen.Unquote

type Env =
    { executeCommand: string -> string -> unit Async
      executeCommandOnce: string -> string -> unit Async
      waitForMessage: string -> unit Async
      setDownloadStage: int -> unit
      resetApplication: unit -> unit }

let private assertBot
    repeat
    (input: Threading.Channels.Channel<string>)
    (output: Threading.Channels.Channel<string>)
    msg
    expected
    =
    let rec flaky count f =
        async {
            try
                do! f
            with
            | e ->
                if count > 0 then
                    do! Async.Sleep 1_000
                    do! flaky (count - 1) f
                else
                    raise e
        }

    let prev = ref "<not set>"

    let read () =
        let (success, v) = output.Reader.TryRead()

        if success then
            prev := v
            v
        else
            !prev

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

let mkDownloadString stage url =
    IO.Path.Combine(
        IO.Directory.GetCurrentDirectory(),
        sprintf "../../../../tests/examples/%i/" stage,
        Uri.EscapeDataString(string url)
    )
    |> IO.File.ReadAllText
    |> async.Return

let private mkApplication (output: string Channel) (input: string Channel) db downloadString time =
    Spectator.Application.mkApplication
        (fun userId message ->
            output.Writer.WriteAsync message |> ignore
            async.Return())
        (async {
            let! m = input.Reader.ReadAsync()
            return "0", m
        })
        (fun url -> ! downloadString url)
        false
        time
        db

let run f =
    let mkState () =
        {| output = Channel.CreateUnbounded<string>()
           input = Channel.CreateUnbounded<string>()
           downloadString = ref <| mkDownloadString 0 |}

    let mutable state = mkState ()

    let dbPath = IO.Path.GetTempFileName()

    let mkApp () =
        mkApplication state.output (state).input dbPath state.downloadString (TimeSpan.FromSeconds 5.0)

    async {
        let! _ = mkApp () |> Async.StartChild

        do!
            f
                { executeCommand = fun cmd expected -> assertBot true state.input state.output cmd expected
                  executeCommandOnce = fun cmd expected -> assertBot false state.input state.output cmd expected
                  waitForMessage = fun expected -> assertBot false state.input state.output "" expected
                  setDownloadStage = fun stage -> state.downloadString.Value <- mkDownloadString stage
                  resetApplication =
                    fun _ ->
                        state <- mkState ()
                        mkApp () |> Async.Start }
    }
    |> Async.RunSynchronously
