module Spectator.Store.MiniKafka

module Atoms =
    open System.Threading

    type Atom<'T when 'T : not struct>(value : 'T) =
        let refCell = ref value
        let rec swap f = 
            let currentValue = !refCell
            let result = Interlocked.CompareExchange<'T>(refCell, f currentValue, currentValue)
            if obj.ReferenceEquals(result, currentValue) then result
            else Thread.SpinWait 20; swap f
        member _.Value with get() = !refCell
        member _.Swap (f : 'T -> 'T) = swap f

    let atom value = 
        new Atom<_>(value)
    let (!) (atom : Atom<_>) =  
        atom.Value
    let swap (atom : Atom<_>) (f : _ -> _) =
        atom.Swap f

open System
open Atoms

type 'e t = { channels : Threading.Channels.Channel<'e> list Atom }
type 'e t2 = { channel : Threading.Channels.Channel<'e> }

let createGroup () : _ t = { channels = atom [] }

let sendEvent (t : _ t) (e : _) : unit Async =
    async {
        let channels = !t.channels
        for ch in channels do
            do! (ch.Writer.WriteAsync e).AsTask() |> Async.AwaitTask 
    }

let private receiveEvent' (t : _ t2) : _ Async =
    async {
        return! (t.channel.Reader.ReadAsync()).AsTask() |> Async.AwaitTask 
    }

let private createReader' (t : _ t) : _ t2 =
    let t2 = { channel = Threading.Channels.Channel.CreateUnbounded() }
    swap t.channels (fun channels -> t2.channel :: channels) |> ignore
    t2

let createReader (t : _ t) : _ Async =
    let r = createReader' t
    receiveEvent' r
