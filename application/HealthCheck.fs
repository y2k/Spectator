module Spectator.HealthCheck

open System
open Spectator.Core

type HealthCheckRequested =
    | HealthCheckRequested of Guid
    interface Event

type SendHealthCheckResponse =
    | SendHealthCheckResponse of Guid
    interface Command

let handleEvent (e: Event) : Command list =
    match e with
    | :? HealthCheckRequested as HealthCheckRequested id -> [ SendHealthCheckResponse id ]
    | _ -> []

open System.Net

let private sendResponseToClient (text: string) (ctx: HttpListenerContext) =
    async {
        use _ = ctx.Response
        let bytes = Text.Encoding.UTF8.GetBytes text

        do!
            ctx.Response.OutputStream.WriteAsync(bytes, 0, bytes.Length)
            |> Async.AwaitTask
    }

let handleCmd (atom: Atom.IAtom<_>) (cmd: Command) =
    match cmd with
    | :? SendHealthCheckResponse as SendHealthCheckResponse id ->
        atom.dispatch (fun db -> Map.remove id db, Map.tryFind id db)
        |> Option.iter (sendResponseToClient "OK" >> Async.Start)
    | _ -> ()

let main (dispatch: Event -> unit) =
    async {
        let listener = new HttpListener()
        listener.Prefixes.Add "http://localhost:8888/"
        listener.Start()

        let atom: Map<Guid, HttpListenerContext> Atom.IAtom = Atom.atom Map.empty

        while true do
            let! ctx = listener.GetContextAsync()

            let id = Guid.NewGuid()
            atom.update (Map.add id ctx)

            dispatch (HealthCheckRequested id)
    }
