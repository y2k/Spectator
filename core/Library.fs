namespace Spectator.Core

module AsyncChannel =
    open System.Threading.Channels

    type 't t = private { chan: 't Channel }

    let make () = { chan = Channel.CreateUnbounded() }

    let write { chan = chan } x = chan.Writer.TryWrite x |> ignore

    let read { chan = chan } =
        (chan.Reader.ReadAsync()).AsTask() |> Async.AwaitTask
