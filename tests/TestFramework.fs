module TestFramework

open System
open System.Threading.Channels
open Swensen.Unquote

module MockDatabase =
    open MongoDB.Bson

    let make () : Map<string, BsonDocument list> ref = ref Map.empty

    let saveObject state name (x: BsonDocument) =
        let col =
            Map.tryFind name !state |> Option.defaultValue []

        state := Map.add name (x :: col) !state

    let delete state name id =
        let col =
            Map.tryFind name !state
            |> Option.defaultValue []
            |> List.filter (fun (x: BsonDocument) -> x.["_id"].AsGuid <> id)

        state := Map.add name col !state

    let queryAll state name callback =
        async {
            let col =
                Map.tryFind name !state |> Option.defaultValue []

            for doc in col do
                do! callback doc
        }

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
            with e ->
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

let private mkApplication (output: string Channel) (input: string Channel) db downloadString =
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
        (fun name o ->
            MockDatabase.saveObject db name o
            async.Zero())
        (fun name id ->
            MockDatabase.delete db name id
            async.Zero())
        (fun name callback -> MockDatabase.queryAll db name callback)

let run f =
    let db = MockDatabase.make ()

    let mkState () =
        {| output = Channel.CreateUnbounded<string>()
           input = Channel.CreateUnbounded<string>()
           downloadString = ref <| mkDownloadString 0 |}

    let state = ref <| mkState ()

    let mkApp () =
        mkApplication (!state).output (!state).input db (!state).downloadString (TimeSpan.FromSeconds 5.0)

    async {
        let! _ = mkApp () |> Async.StartChild

        do!
            f
                { executeCommand = fun cmd expected -> assertBot true (!state).input (!state).output cmd expected
                  executeCommandOnce = fun cmd expected -> assertBot false (!state).input (!state).output cmd expected
                  waitForMessage = fun expected -> assertBot false (!state).input (!state).output "" expected
                  setDownloadStage = fun stage -> (!state).downloadString := mkDownloadString stage
                  resetApplication =
                      fun _ ->
                          state := mkState ()
                          mkApp () |> Async.Start }
    }
    |> Async.RunSynchronously
