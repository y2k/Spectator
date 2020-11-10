module Spectator.Core.Tea

type t<'e> =
    { update: 'e list -> unit
      effect: ((('e list -> unit) -> unit) -> unit Async) -> unit Async }

let make (initState: 'state)
         (merge: 'state -> 'event -> 'state)
         (f: ((('state -> 'state * 'event list)) -> 'state Async) -> unit Async)
         =
    let state = ref initState
    { update =
          fun (es: 'event list) ->
              for e in es do
                  state := merge !state e
      effect =
          fun (updateAll: (('event list -> unit) -> unit) -> unit Async) ->
              let er =
                  (fun g ->
                      async {
                          let result = ref None
                          do! updateAll (fun updateOther ->
                                  let c = !state
                                  let (a, b) = g c
                                  state := a
                                  if List.isNotEmpty b then updateOther b
                                  result := Some c)

                          return !result |> Option.get
                      })

              f er }

let run (xs: 'e t list) =
    let mutex = new System.Threading.SemaphoreSlim 1
    xs
    |> List.map (fun x ->
        async {
            while true do
                do! x.effect (fun syncLinkedStates ->
                        async {
                            do! mutex.WaitAsync() |> Async.AwaitTask
                            syncLinkedStates (fun events ->
                                for e in xs do
                                    e.update events)
                            mutex.Release() |> ignore
                        })
        })
    |> Async.Parallel
    |> Async.Ignore
