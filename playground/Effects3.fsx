#r "nuget: SodiumFRP.FSharp, 5.0.6"

open Sodium.Frp

type DownloadEffect = DownloadEffect of url: string * callbackEffect: obj

type UpdateStateFx = UpdateStateFx of data: obj * cell: obj

type NewTelegramMessageFx = NewTelegramMessageFx of user: string * message: string
type SubScreatedFx = SubScreatedFx of sub : obj

module Domain =
    type State = { subs : string list }

    let private sState = { subs = []}

    let handleGLobalMsg (state : State) (e: SubScreatedFx) : obj list =
        [ UpdateStateFx (state, sState) ]
