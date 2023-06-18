#r "nuget: Telegram.Bot, 19.0.0-preview.2"
#r "nuget: FSharp.Interop.Dynamic, 5.0.1.268"

module Applicative =
    let pure_ f = async { return f }

    let apply fa ff =
        async {
            let! f = ff
            let! a = fa
            return f a
        }

module BotTypes =
    type TelegramMessage = TelegramMessage of user: string * message: string
    type TelegramCommand = TelegramCommand of user: string * message: string

module DomainMessages =
    type NewSubscription = { user: string; url: string }
    type Subscription = { user: string; url: string }
    type CreateNewSubscription = CreateNewSubscription of NewSubscription
    type CreateSubscription = CreateSubscription of Subscription

module BotDomain =
    open BotTypes
    open DomainMessages

    type State =
        { subscriptions: Map<string, NewSubscription list> }

        static member empty = { subscriptions = Map.empty }

    let stateReduce (state: State) (command: obj) : State =
        match command with
        | :? CreateNewSubscription as (CreateNewSubscription ns) ->
            let subs =
                ns :: (state.subscriptions |> Map.tryFind ns.user |> Option.defaultValue [])

            { state with subscriptions = Map.add ns.user subs state.subscriptions }
        | _ -> state

    let handle (state: State) (TelegramMessage (user, msg)) : obj list =
        match msg.Split ' ' with
        | [| "/ls" |] ->
            let message =
                state.subscriptions
                |> Map.tryFind user
                |> Option.defaultValue []
                |> Seq.fold (fun a x -> sprintf "%s\n- %s" a x.url) "Subs:"

            [ TelegramCommand(user, message) ]
        | [| "/add"; url |] ->
            [ CreateNewSubscription { user = user; url = url }
              TelegramCommand(user, "Subscription created") ]
        | _ -> [ TelegramCommand(user, $"Unknown CMD: %s{msg}") ]

    let handle_ a b = handle b a

module HttpMessagesCommands =
    type DownloadCommand = DownloadCommand of string list * ((string * Result<byte[], exn>) list -> obj)

module LogCommands =
    type Log = Log of string

module CreateSubscriptionDomain =
    open DomainMessages
    module H = HttpMessagesCommands

    type State =
        { newSubs: NewSubscription list
          counter: int }

        static member empty = { newSubs = []; counter = 0 }

    let reduceState (state: State) (cmd: obj) : State =
        match cmd with
        | :? CreateNewSubscription as (CreateNewSubscription ns) -> { state with newSubs = ns :: state.newSubs }
        | _ -> state

    type DownloadComplete = DownloadComplete of (string * Result<byte[], exn>) list

    let reduceEvent (state: State) : obj list =
        let urls = state.newSubs |> List.map (fun ns -> ns.url)

        [ H.DownloadCommand(urls, (fun r -> DownloadComplete r))
          { state with counter = state.counter + 1 }
          LogCommands.Log $"(LOG)[USER] Time Event handled [%A{state}]" ]

    let reduceDownloadEvent (DownloadComplete results) : obj list =
        [ LogCommands.Log "(LOG) DownloadComplete" ]

module TelegramApi =
    open BotTypes
    open Telegram.Bot
    open Telegram.Bot.Types

    let sendToTelegram (token: string) (TelegramCommand (user, message)) =
        let client = TelegramBotClient token

        client.SendTextMessageAsync(ChatId.op_Implicit user, message, parseMode = Enums.ParseMode.Html)
        |> ignore

    let produce (token: string) (dispatch: TelegramMessage -> unit) =
        async {
            let mutable offset = 0
            let client = TelegramBotClient(token)

            while true do
                let! updates =
                    client.GetUpdatesAsync(limit = 10, offset = offset, timeout = 60)
                    |> Async.AwaitTask

                offset <- updates |> Seq.fold (fun a x -> max a (x.Id + 1)) offset

                updates
                |> Seq.iter (fun x -> dispatch (TelegramMessage(string x.Message.From.Id, x.Message.Text)))
        }

module LogConsumer =
    let handleCmd (cmd: obj) = printfn "LOG[CMD] :: %A" cmd

    let handleMsg (msg: obj) =
        printfn "LOG[MSG] :: %A" msg
        []

module StateHolder =
    type 'a t = private { mutable value: 'a }
    let init x = { value = x }
    let makeGet t = async { return t.value }

    let decorate (reduce: 'a -> 'e -> 'a) (state: 'a t) (a: obj list Async) =
        async {
            let! r = a

            return
                r
                |> List.choose (fun r ->
                    match r with
                    | :? 'e as x ->
                        let s1 = state.value
                        let s2 = reduce s1 x

                        if s1 = s2 then
                            Some r
                        else
                            state.value <- s2
                            None
                    | _ -> Some r)
        }

module MsgModule =
    open System.Threading.Channels

    type t =
        private
            { mutable listeners: obj Channel list }

    let make () = { listeners = [] }

    let register (t: t) : 'a Async =
        let ch = Channel.CreateBounded 1
        t.listeners <- ch :: t.listeners

        let rec read () =
            async {
                let! value = ch.Reader.ReadAsync().AsTask() |> Async.AwaitTask

                match value with
                | :? 'a as x -> return x
                | _ -> return! read ()
            }

        read ()

    let makeDispatch t (msg: obj) =
        async {
            for ch in t.listeners do
                do! (ch.Writer.WriteAsync msg).AsTask() |> Async.AwaitTask
        }
        |> Async.RunSynchronously

module TimerProducer =
    type TimerEvent = TimerEvent

    let produce dispatch =
        async {
            while true do
                do! Async.Sleep 5_000
                dispatch TimerEvent
        }

module Framework =
    let startApp mkRoutes handleOne mkProducers =
        let handle_ handleOne (commands: obj list seq) =
            commands
            |> Seq.concat
            |> Seq.map (fun x ->
                handleOne x
                x)
            |> Seq.map (fun x ->
                LogConsumer.handleCmd x
                x)
            |> Seq.iter ignore

        let dispatchT = MsgModule.make ()

        mkRoutes dispatchT
        |> List.mapi (fun i a ->
            async {
                while true do
                    // printfn "LOG[%i] :: BEGIN" (i + 1)
                    let! cmds = a
                    handle_ handleOne [ cmds ]
                    // printfn "LOG[%i] :: END" (i + 1)
                    ()
            })
        |> Async.Parallel
        |> Async.Ignore
        |> Async.Start

        let dispatch (x: obj) = MsgModule.makeDispatch dispatchT x

        mkProducers dispatch |> Async.Parallel |> Async.Ignore |> Async.RunSynchronously

module Applcation =
    open BotTypes

    let attachState empty stateReduce a =
        let state = StateHolder.init empty in

        a
        |> Applicative.apply (StateHolder.makeGet state)
        |> StateHolder.decorate stateReduce state

    let () =
        let mkRoutes dispatchT =
            [ Applicative.pure_ BotDomain.handle_
              |> Applicative.apply (MsgModule.register dispatchT)
              |> attachState BotDomain.State.empty BotDomain.stateReduce

              Applicative.pure_ (fun (TimerProducer.TimerEvent _) -> CreateSubscriptionDomain.reduceEvent)
              |> Applicative.apply (MsgModule.register dispatchT)
              |> attachState CreateSubscriptionDomain.State.empty CreateSubscriptionDomain.reduceState

              Applicative.pure_ LogConsumer.handleMsg
              |> Applicative.apply (MsgModule.register dispatchT) ]

        let token = ""

        let mkProducers =
            fun (dispatch: obj -> unit) -> [ TelegramApi.produce token dispatch; TimerProducer.produce dispatch ]

        let handleCmd (x: obj) =
            match x with
            | :? TelegramCommand as cmd -> TelegramApi.sendToTelegram token cmd
            | _ -> ()

        Framework.startApp mkRoutes handleCmd mkProducers
