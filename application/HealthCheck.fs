module Spectator.HealthCheck

open Spectator.Core
open System.Net
open System.Text

type State =
    { healthCheckComplete: bool }
    static member Empty = { healthCheckComplete = false }

let updatePing state =
    function
    | HealthCheckRequested -> { healthCheckComplete = true }
    | NewSubscriptionCreated _
    | SubscriptionCreated _
    | SubscriptionRemoved _
    | SnapshotCreated _ -> state

type t = { ctx: HttpListenerContext }

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

        do! ctx.Response.OutputStream.WriteAsync(bytes, 0, bytes.Length)
            |> Async.AwaitTask

        ctx.Response.Close()
    }

let main startServer sendText update =
    let waitForPong =
        async {
            let stop = ref false

            while !stop do
                let! breakLoop =
                    update (fun db -> db, [])
                    >>- fun db -> db.healthCheckComplete

                stop := breakLoop
                if not breakLoop then do! Async.Sleep 1_000
        }

    async {
        let ctxFactory = startServer "http://localhost:8888/"

        while true do
            let! ctx = ctxFactory

            do! update (fun db -> { healthCheckComplete = false }, [ HealthCheckRequested ])
                |> Async.Ignore

            do! waitForPong

            do! sendText ctx "OK"
    }