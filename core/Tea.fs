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
