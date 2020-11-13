module Spectator.Core.Tea

module Tea =
    open System.Threading

    type 'e t =
        { mutex: SemaphoreSlim
          xs: ('e list -> unit) list ref }

    let init () =
        { mutex = new SemaphoreSlim(1)
          xs = ref [] }

    let make (t: 'event t) (initState: 'state) (merge: 'state -> 'event -> 'state) =
        let state = ref initState

        let update es =
            for e in es do
                state := merge !state e

        t.xs := update :: !t.xs

        fun f ->
            async {
                do! t.mutex.WaitAsync() |> Async.AwaitTask

                let oldState = !state
                let (state', es) = f oldState
                state := state'

                for u in !t.xs do
                    u es

                t.mutex.Release() |> ignore
                return oldState
            }
