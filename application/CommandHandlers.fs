namespace Spectator

open System
open Spectator.Core

module StoreWrapper =
    let makeDispatch (handleMsg: Event -> Command list Async) (handleCmd: (Event -> unit) -> Command -> unit) =
        let mail: MailboxProcessor<Event> =
            MailboxProcessor.Start(fun mail ->
                async {
                    while true do
                        let! msg = mail.Receive()

                        try
                            let! cmds = handleMsg msg
                            cmds |> Seq.iter (handleCmd mail.Post)
                        with e ->
                            eprintfn "ERROR: %O" e
                            exit -1
                })

        mail.Post

module StoreAtom =
    type 's StateStore = { mutable state: 's }

    let inline make () : ^state StateStore =
        { state = (^state: (static member empty: ^state) ()) }

    let make_ empty : _ StateStore = { state = empty }

    let addStateCofx (state: _ StateStore) f = fun x -> f state.state x

    let handleCommand (stateHolder: 'state StateStore) (cmd: Command) =
        match cmd with
        | :? 'state as newState -> stateHolder.state <- newState
        | _ -> ()

    let handleCommandFun (stateHolder: 'state StateStore) update (cmd: Command) =
        stateHolder.state <- update stateHolder.state cmd

module Router =
    let makeHandleEvent (f: 'State -> 'e -> _) =
        fun (state: 'State) (e: Event) ->
            match e with
            | :? 'e as e2 -> f state e2
            | _ -> []

    let makeHandleEvent_ handleEvent' (e: Event) : Command list =
        match e with
        | :? _ as x -> handleEvent' x
        | _ -> []

    type t =
        { eventHandlers: (Async<Event -> Command list>) list
          commandHandlers: ((Event -> unit) -> Command -> unit) list
          eventGenerators: ((Event -> unit) -> unit Async) list }

    let init =
        { eventHandlers = []
          commandHandlers = []
          eventGenerators = [] }

    // [<Obsolete>]
    // let inline addStatefull handleEvent handleStateCmd (t: t) : t =
    //     let botState = StoreAtom.make ()

    //     let eh e =
    //         (StoreAtom.addStateCofx botState handleEvent) e

    //     let ch1 _ cmd = StoreAtom.handleCommand botState cmd

    //     let ch2 _ cmd =
    //         StoreAtom.handleCommandFun botState handleStateCmd cmd

    //     { t with
    //         eventHandlers = async.Return eh :: t.eventHandlers
    //         commandHandlers = ch1 :: ch2 :: t.commandHandlers }

    // [<Obsolete>]
    // let inline addStatefull_ handleEvent handleStateCmd (t: t) : t =
    //     let botState = StoreAtom.make ()

    //     let eh e =
    //         (StoreAtom.addStateCofx botState handleEvent) e

    //     let ch2 _ cmd =
    //         StoreAtom.handleCommandFun botState handleStateCmd cmd

    //     { t with
    //         eventHandlers = async.Return eh :: t.eventHandlers
    //         commandHandlers = ch2 :: t.commandHandlers }

    let addEvent eventHandler (t: t) : t =
        { t with eventHandlers = async.Return eventHandler :: t.eventHandlers }

    let addCommand commandHandler (t: t) : t =
        { t with commandHandlers = commandHandler :: t.commandHandlers }

    let addCommand_ commandHandler (t: t) : t =
        { t with commandHandlers = (fun _ cmd -> commandHandler cmd) :: t.commandHandlers }

    let addEventGenerator eventGen (t: t) : t =
        { t with eventGenerators = eventGen :: t.eventGenerators }

    let makeCommandDispatch (t: t) =
        let handleCommand dispatch cmd =
            t.commandHandlers
            |> List.rev
            |> List.iter (fun commandHandler -> commandHandler dispatch cmd)

        handleCommand ignore

    let start startEvent (t: t) : unit Async =
        let handleEvent e =
            async {
                let! fs = t.eventHandlers |> List.rev |> Async.Sequential
                return fs |> Seq.collect (fun eventHandler -> eventHandler e) |> List.ofSeq
            }

        let handleCommand dispatch cmd =
            t.commandHandlers
            |> List.rev
            |> List.iter (fun commandHandler -> commandHandler dispatch cmd)

        let dispatch = StoreWrapper.makeDispatch handleEvent handleCommand
        dispatch startEvent
        printfn "Started..."

        t.eventGenerators
        |> List.map (fun eventGen -> eventGen dispatch)
        |> Async.loopAll

module Aplicative =
    let pure_ f = async { return f }

    let apply ff fa =
        async {
            let! f = ff
            let! a = fa
            return f a
        }

module EventLocker =
    let decorateWithLock (f: Event -> Command list) : Event -> Command list =
        let locked = Atom.atom (ref false)

        fun e ->
            if locked.Value.Value then
                match e with
                | :? Initialize -> []
                | e ->
                    f e
                    |> List.choose (function
                        | :? InitializeCompleted ->
                            locked.update (fun _ -> ref false)
                            None
                        | cmd -> Some cmd)
            else
                match e with
                | :? Initialize -> locked.update (fun _ -> ref true)
                | _ -> ()

                f e

module AsyncRouter =
    type t<'a when 'a: not struct> = private { state: 'a Atom.IAtom }
    let make empty = { state = Atom.atom empty }
    // let decorateEventHandler { state = botState } handleEvent = fun e -> handleEvent botState.Value e

    let decorateEventHandler { state = botState } handleEvent : Event -> Command list =
        fun (e: Event) ->
            handleEvent botState.Value e
            |> List.choose (fun (cmd: Command) ->
                match cmd with
                | :? 'state as newState ->
                    botState.update (fun _ -> newState)
                    None
                | _ -> Some cmd)

    // let decorateCommandHandler' { state = botState } handleStateCmd =
    //     let ch1 _ (cmd: Command) =
    //         match cmd with
    //         | :? 'state as newState -> botState.update (fun _ -> newState)
    //         | _ -> ()

    //     let ch2 _ (cmd: Command) =
    //         botState.update (fun x -> handleStateCmd x cmd)

    //     fun d cmd -> ch1 :: ch2 :: [] |> List.iter (fun f -> f d cmd)

    // let decorateCommandHandler { state = botState } handleStateCmd =
    //     let ch1 _ (cmd: Command) =
    //         match cmd with
    //         | :? 'state as newState -> botState.update (fun _ -> newState)
    //         | _ -> ()

    //     let ch2 _ (cmd: Command) =
    //         botState.update (fun x -> handleStateCmd x cmd)

    //     fun d cmd -> ch1 :: ch2 :: [] |> List.iter (fun f -> f d cmd)

    let makeCommandHandler { state = botState } handleStateCmd =
        let ch2 _ (cmd: Command) =
            botState.update (fun x -> handleStateCmd x cmd)

        // fun d cmd -> ch2 :: [] |> List.iter (fun f -> f d cmd)
        ch2

module TelegramEventAdapter =
    let handleCommand sendToTelegram (cmd: Command) =
        match cmd with
        | :? SendTelegramMessage as SendTelegramMessage (user, msg) ->
            sendToTelegram user msg |> Async.Ignore |> Async.Start
        | _ -> ()

    let generateEvents readFromTelegram (dispatch: Event -> unit) =
        async {
            while true do
                let! (user: string, msg: string) = readFromTelegram
                dispatch (TelegramMessageReceived(user, msg))
        }

// module SheduleGenerator =
//     let dispatchWithTimeout (dispatch: Event -> unit) (cmd: Command) =
//         match cmd with
//         | :? DispatchWithTimeout as DispatchWithTimeout (t, e) ->
//             async {
//                 do! Async.Sleep t
//                 dispatch e
//             }
//             |> Async.Start
//         | _ -> ()

//     let dispatchWithInterval (dispatch: Event -> unit) (cmd: Command) =
//         match cmd with
//         | :? DispatchWithInterval as DispatchWithInterval (t, e) ->
//             async {
//                 while true do
//                     do! Async.Sleep t
//                     dispatch e
//             }
//             |> Async.Start
//         | _ -> ()

module Https =
    open System.Net.Http

    let download (uri: Uri) =
        async {
            use client = new HttpClient()

            "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/605.1.15 (KHTML, like Gecko) Chrome/71.0.3578.98 Safari/605.1 Edge/19.17763"
            |> client.DefaultRequestHeaders.UserAgent.ParseAdd

            let! result = client.GetByteArrayAsync uri |> Async.AwaitTask |> Async.Catch

            return
                match result with
                | Choice1Of2 x -> Ok x
                | Choice2Of2 e -> Error e
        }

    let handleCommand downloadString dispatch (cmd: Command) =
        match cmd with
        | :? DownloadHttp as DownloadHttp (uris, callback) ->
            async {
                let! result =
                    uris
                    |> Seq.map (fun uri -> downloadString uri)
                    |> fun xs -> Async.Parallel(xs, 3)

                dispatch (callback (List.ofSeq result))
            }
            |> Async.Start
        | _ -> ()

// [<Obsolete>]
// module RouterUtils =
//     type DispatchWithTimeoutCallback =
//         | DispatchWithTimeoutCallback of Guid
//         interface Event

//     let handleEvent appId handleTimerEvent handleDownloadEvent state (e: Event) : Command list =
//         match e with
//         | :? Initialize -> handleTimerEvent state
//         | :? DispatchWithTimeoutCallback as DispatchWithTimeoutCallback id when id = appId -> handleTimerEvent state
//         | e ->
//             handleDownloadEvent state e
//             |> List.map (fun (cmd: Command) ->
//                 match cmd with
//                 | :? NotifyTransactionEnded ->
//                     DispatchWithTimeout(TimeSpan.FromMinutes 1, DispatchWithTimeoutCallback appId)
//                 | cmd -> cmd)
