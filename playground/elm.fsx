module Prelude =
    type Element = Element
    let div (_: _ list) (_: Element list) : Element = failwith "???"
    let span (_: _ list) (_: Element list) : Element = failwith "???"
    let str (_: string) : Element = failwith "???"
    let onClick (dispatch: 'msg -> unit) (msg: 'msg) = fun _ -> dispatch msg

    module List =
        let at _ (_: 'x list) : 'x = failwith "???"
        let replaceAt _ (_: 'x) (_: 'x list) : 'x list = failwith "???"

open Prelude

module CouterView =
    type Model = int

    type Msg =
        | Inc
        | Dec

    let update (model: Model) (msg: Msg) : Model =
        match msg with
        | Inc -> model + 1
        | Dec -> model - 1

    let view (model: Model) (dispatch: Msg -> unit) : Element =
        div
            []
            [ div [ onClick dispatch Inc ] [ str "+" ]
              span [] [ str (string model) ]
              div [ onClick dispatch Dec ] [ str "-" ] ]

module ParentView =
    module C = CouterView

    type Model = { children: C.Model list }
    type Msg = ChildMsd of int * C.Msg

    let update (model: Model) (msg: Msg) : Model =
        match msg with
        | ChildMsd(i, cmsg) ->
            let cmodel = C.update (List.at i model.children) cmsg

            { model with
                children = model.children |> List.replaceAt i cmodel }

    let view (model: Model) (dispatch: Msg -> unit) : Element =
        div
            []
            (model.children
             |> List.mapi (fun i cmodel -> C.view cmodel (fun cmsg -> dispatch (ChildMsd(i, cmsg)))))
