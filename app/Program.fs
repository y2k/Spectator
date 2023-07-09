[<CompilerMessage("Incomplete hole", 130)>]
let inline FIXME (x: _) = raise (exn $"Incomplete hole: {x}")

type EffectArg = { param: obj; action: unit -> unit }
type World = unit
type Effect = World -> EffectArg

let merge (_: Effect list) : Effect = FIXME ""

module TelegramApi =
    let sendMessage (user: string) (message: string) =
        fun (_: World) ->
            { param = box (user, message)
              action = FIXME "" }

module GlobalApi =
    let dispatch (msg: _) =
        fun (_: World) -> { param = box msg; action = FIXME "" }

module Global =
    type NewSubscriptionCreated = NewSubscriptionCreated of user: string * url: string

module Bot =
    open Global

    type State = { subs: Map<string, string list> }

    let handle (state: State) (user, (message: string)) : Effect =
        match message.Split ' ' with
        | [| "/ls" |] ->
            state.subs
            |> Map.tryFind user
            |> Option.defaultValue []
            |> List.fold (fun s a -> $"{a}\n{s}") "Your subs:"
            |> TelegramApi.sendMessage user
        | [| "/add"; url |] -> merge [ GlobalApi.dispatch (NewSubscriptionCreated(user, url)) ]
        | _ -> FIXME ""

let () = printfn "Hello from F#"
