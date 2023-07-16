#r "nuget: FSharp.SystemTextJson, 1.1.23"

open System.Text.Json
open System.Text.Json.Serialization

module Effects =
    type Effect =
        interface
        end

    type Effects = Effect list

    let emptyFx: Effects = []

    let batch (fs: Effects list) : Effects = List.concat fs

open Effects

module TelegramApi =
    type SendMessage =
        | SendMessage of user: string * message: string
        interface Effect

    let sendMessage (user: string) (message: string) : obj = [ SendMessage(user, message) ]

let cmdLog = TelegramApi.sendMessage "y2k" "hello"

let options = JsonSerializerOptions(WriteIndented = true)
let a = JsonFSharpOptions.Default()
// // a.UnionEncoding <- JsonUnionEncoding.Inherit
a.AddToJsonSerializerOptions(options)
let actual = JsonSerializer.Serialize(cmdLog, options)

printfn "%s" actual
