#r "nuget: SodiumFRP.FSharp, 5.0.6"

open Sodium.Frp

module Core =
    type Msg =
        interface
        end

    type Cmd =
        interface
        end

module Domain =
    open Core

    type State1 = State1
    type State2 = State2
    let handle (msg: Msg) (state1: State1) (state2: State2) : Cmd list = []

module TelegramApi =
    open Core

    type TelegramEvent =
        { user: string
          message: string }

        interface Msg

    let events: TelegramEvent Stream = StreamSink.create ()

let c = CellSink.create 0
CellSink.send
Cell.listen
Cell.listenWeak
Cell.sample

let b = BehaviorSink.create 0
BehaviorSink.send
Behavior.sample

StreamSink.create
StreamSink.send
Stream.listen
Stream.listenWeak
Stream.listenOnce
Stream.listenOnceAsync
// Stream.sample - NO

let s1 = BehaviorSink.create Domain.State1
let s2 = BehaviorSink.create Domain.State2

// let _a = Behavior.lift2 Domain.handle (s1, s2)

let _a = Stream.snapshot2B s1 s2 Domain.handle TelegramApi.events
Stream.snapshotAndTakeB
