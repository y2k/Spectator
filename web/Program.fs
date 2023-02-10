module Spectator.Web

open Suave
open Suave.Filters
open Suave.Operators
open Spectator.Core

type WebRequest =
    | WebRequest of byte[]
    interface Event

let start (dispatch: Event -> unit) =
    choose
        [ pathStarts "/api"
          >=> POST
          >=> request (fun r ->
              dispatch (WebRequest r.rawForm)
              Successful.NO_CONTENT) ]
    |> startWebServerAsync { defaultConfig with bindings = [ HttpBinding.createSimple HTTP "0.0.0.0" 8080 ] }
    |> snd
