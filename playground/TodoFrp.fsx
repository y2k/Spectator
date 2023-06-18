#r "nuget: SodiumFRP.FSharp, 5.0.6"
#r "nuget: FsHtml, 0.2.0"

module Signal =
    open Sodium.Frp

    let makeEvent = StreamSink.create

    let makeStore defValue (streams: _ Stream seq) =
        Stream.mergeAll (fun _ _ -> failwith "???") streams
        |> Stream.accum defValue (fun eventValue oldValue -> eventValue oldValue)
        |> Cell.asBehavior

    let map = Behavior.map
    let on textChanged f = textChanged |> Stream.map f
    let on2 addClicked textStore f = Stream.snapshotB textStore f addClicked

    let mergeStore newStore prevStore =
        Behavior.lift2 (fun a b -> (a, b)) (prevStore, newStore)

let textChanged = Signal.makeEvent ()
let addClicked = Signal.makeEvent ()

let uiStore =
    let textStore =
        Signal.makeStore
            ""
            [ Signal.on textChanged (fun newValue _ -> newValue)
              Signal.on addClicked (fun _ _ -> "") ]

    textStore
    |> Signal.mergeStore (
        Signal.makeStore [] [ Signal.on2 addClicked textStore (fun _ text oldItems -> oldItems @ [ text ]) ]
    )
    |> Signal.mergeStore (Signal.map ((<>) "") textStore)
    |> Signal.map (sprintf "%A")

module Tests =
    open Sodium.Frp

    let () =
        let test expected =
            let actual = (Behavior.sample uiStore).Replace("\n", ";")

            if actual <> expected then
                failwithf "%s <> %s" actual expected

        StreamSink.send "item #1" textChanged
        StreamSink.send "item #2" textChanged
        test """(("item #2", []), true)"""
        StreamSink.send () addClicked
        test """(("", ["item #2"]), false)"""
        StreamSink.send "item #3" textChanged
        StreamSink.send () addClicked
        test """(("", ["item #2"; "item #3"]), false)"""
