module Spectator.Core.Tea

type t<'e> =
    { update : 'e list -> unit
      effect : ((('e list -> unit) -> unit) -> unit Async) -> unit Async }

let make (initState : 'state) (merge : 'state -> 'event -> 'state) (f : EffectReducer<'state, 'event> -> unit Async) =
    let state = ref initState
    { update = fun (es : 'event list) ->
                   for e in es do state := merge !state e
      effect = fun (updateAll : (('event list -> unit) -> unit) -> unit Async) ->
                   let er =
                       { new EffectReducer<'state, 'event> with
                             member __.invoke g =
                               async {
                                   let result = ref None
                                   do! updateAll (fun updateOther ->
                                       let (a, b, c) = g !state
                                       state := a
                                       updateOther b
                                       result := Some c
                                   )
                                   return !result |> Option.get
                               }
                       }
                   f er }

let run (xs : 'e t list) =
    let mutex = new System.Threading.SemaphoreSlim 1
    xs
    |> List.map (fun x ->
        async {
            while true do
                do! x.effect (fun _c ->
                    async {
                        do! mutex.WaitAsync () |> Async.AwaitTask

                        _c (fun events ->
                                for e in xs do
                                    e.update events)

                        mutex.Release () |> ignore
                    }) })
    |> Async.Parallel
    |> Async.Ignore
