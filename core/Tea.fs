module Spectator.Core.Tea

let start initState initCmd update mkMsg executeEffect readEvent =
    let state = ref initState

    let rec loopUpdate msg =
        async {
            let (s2, effs) = update msg !state
            state := s2
            do! effs
                |> List.map (executeEffect >=> loopUpdate)
                |> Async.Parallel
                |> Async.Ignore
        }

    [ async {
          let cmd = initCmd
          for eff in cmd do
              let! msg = executeEffect eff
              do! loopUpdate msg }
      async {
          while true do
              let! e = readEvent
              do! loopUpdate (mkMsg e) } ]
    |> Async.Parallel
    |> Async.Ignore

let runMain (readEvent : 'e Async) (sendEvent : 'e -> unit Async) (initState : 's) updateStore f =
    let state = ref initState
    let syncState : unit Async =
        async {
            while true do
                let! e = readEvent
                state := updateStore !state e
        }
    let syncRunLoop : unit Async =
        let update g =
            async {
                let oldState = !state
                let (s2, es : _ list) = g oldState
                state := s2
                for e in es do
                    do! sendEvent e
                return oldState
            }
        async {
            while true do
                do! f update
        }
    Async.Parallel [ syncState; syncRunLoop ] |> Async.Ignore
