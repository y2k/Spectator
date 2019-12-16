open Suave
open Suave.Swagger.Rest
open Suave.Swagger.Swagger
open Suave.Swagger.FunnyDsl
open Spectator.Core

[<CLIMutable>]
type TimeResult =
    { items : Snapshot [] }

let snapshots = MODEL { items = [||] }

let api =
    swagger {
        for route in getting (simpleUrl "/snapshots" |> thenReturns snapshots) do
            yield route |> addResponse 200 "Featured snapshots" (Some typeof<TimeResult>)
            yield route |> supportsJsonAndXml
    }

[<EntryPoint>]
let main _ =
    startWebServer defaultConfig api.App
    0
