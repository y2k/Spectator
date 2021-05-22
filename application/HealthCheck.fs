module Spectator.HealthCheck

open Spectator.Core
open System.Net
open System.Text

type State =
    { healthCheckComplete: bool }
    static member Empty = { healthCheckComplete = false }

let main startServer sendText (reduce: IReducer<State, Events>) =
    let waitForPong =
        async {
            let stop = ref false

            while !stop do
                let! breakLoop = reduce.Invoke(fun db -> db, [], db.healthCheckComplete)

                stop := breakLoop

                if not breakLoop then
                    do! Async.Sleep 1_000
        }

    async {
        let ctxFactory = startServer "http://localhost:8888/"

        while true do
            let! ctx = ctxFactory

            do! reduce.Invoke(fun db -> { healthCheckComplete = false }, [ HealthCheckRequested ], ())
            do! waitForPong
            do! sendText ctx "OK"
    }

type t = private { ctx: HttpListenerContext }

let startServer url =
    let listener = new HttpListener()
    listener.Prefixes.Add url
    listener.Start()

    async {
        let! ctx = listener.GetContextAsync()
        return { ctx = ctx }
    }

let sendText { ctx = ctx } (text: string) =
    async {
        let bytes = Encoding.UTF8.GetBytes text

        do!
            ctx.Response.OutputStream.WriteAsync(bytes, 0, bytes.Length)
            |> Async.AwaitTask

        ctx.Response.Close()
    }

let updatePing state =
    function
    | HealthCheckRequested -> { healthCheckComplete = true }
    | NewSubscriptionCreated _
    | SubscriptionCreated _
    | SubscriptionRemoved _
    | SnapshotCreated _ -> state

let mainWithDeps makeReducer =
    main startServer sendText (makeReducer State.Empty updatePing)
