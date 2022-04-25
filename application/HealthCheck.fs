module Spectator.HealthCheck

open Spectator.Core
open System.Net
open System.Text

type State =
    { healthCheckRequested: bool }
    static member Empty = { healthCheckRequested = false }

type t = private { ctx: HttpListenerContext }

let private startServer url =
    let listener = new HttpListener()
    listener.Prefixes.Add url
    listener.Start()

    async {
        let! ctx = listener.GetContextAsync()
        return { ctx = ctx }
    }

let private sendResponseToClient { ctx = ctx } (text: string) =
    async {
        let bytes = Encoding.UTF8.GetBytes text

        do!
            ctx.Response.OutputStream.WriteAsync(bytes, 0, bytes.Length)
            |> Async.AwaitTask

        ctx.Response.Close()
    }

let private updatePing state (event: Event) =
    match event with
    | :? HealthCheckRequested -> { state with healthCheckRequested = true }
    | _ -> state

let main (makeReducer: State -> (State -> Event -> State) -> IReducer<State, Event>) =
    let reduce = (makeReducer State.Empty updatePing)

    let ping = reduce.Invoke(fun db -> db, [ HealthCheckRequested ], ())

    let waitForPong =
        async {
            let mutable stop = false

            while stop do
                let! breakLoop =
                    reduce.Invoke(fun db -> { db with healthCheckRequested = false }, [], db.healthCheckRequested)

                stop <- breakLoop

                if not breakLoop then
                    do! Async.Sleep 1_000
        }

    async {
        let ctxFactory = startServer "http://localhost:8888/"

        while true do
            let! ctx = ctxFactory

            do! ping
            do! waitForPong
            do! sendResponseToClient ctx "OK"
    }
