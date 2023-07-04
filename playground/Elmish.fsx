#r "nuget: SodiumFRP.FSharp, 5.0.6"

open System

module Utils =
    open Sodium.Frp

    let inline merge a b = Stream.merge (fun x _ -> x) (a, b)

    [<CompilerMessage("Incomplete hole", 130)>]
    let inline FIXME (x: _) = raise (exn $"Incomplete hole: {x}")

open Utils

module Types =
    type Snapshot =
        { url: string
          subId: string
          data: byte array }

    type Subscription =
        { id: string
          user: string
          url: string
          data: byte array }

    type NewSubscription =
        { id: string
          user: string
          url: string }

    type SubscriptionChanged =
        | NewSubscriptionCreated of NewSubscription
        | NewSubscriptionRemoved of string
        | SubscriptionCreated of Subscription
        | SubscriptionRemoved of user: string * id: string

    type SnapshotCreated = SnapshotCreated of Snapshot

    type DownloadRequested = DownloadRequested of url: string * (byte array -> obj)
    type TelegramMessageReceived = TelegramMessageReceived of user: string * message: string
    type TelegramMessageSended = TelegramMessageSended of user: string * message: string
    type TimeTicked = TimeTicked
    type EventDispatched = EventDispatched of event: obj

open Types

module Elmish =
    type 'msg Sub = unit -> 'msg
    type 'msg Cmd = 'msg Sub list

    let downloadFx (_url: string) (_f: byte array -> 'msg) : 'msg Cmd = failwith "???"
    let dispatchFx (msg: 'a) : 'msg Cmd = failwith "???"
    let mapCmd (_f: 'a -> 'b) (_a: 'a Cmd) : 'b Cmd = failwith "???"
    let batch (_: 'msg Cmd list) : 'msg Cmd = failwith "???"

module SnapshotWorker' =
    open Elmish

    type State = { subs: Subscription list }

    type Msg =
        | TimeTicked
        | AppStateChanged of SubscriptionChanged
        | DownloadComplete of data: byte array * subId: string * url: string

    let update (state: State) msg : State * Msg Cmd =
        match msg with
        | TimeTicked ->
            let fxs =
                state.subs
                |> List.map (fun sub -> downloadFx sub.url (fun data -> DownloadComplete(data, sub.id, sub.url)))

            state, batch fxs
        | DownloadComplete(data, subId, url) ->
            if (state.subs |> List.exists (fun x -> x.id = subId)) then
                let snap: Snapshot =
                    { url = url
                      subId = subId
                      data = data }

                state, dispatchFx (SnapshotCreated snap)
            else
                state, []
        | AppStateChanged msg ->
            match msg with
            | SubscriptionCreated e -> { state with subs = e :: state.subs }, []
            | SubscriptionRemoved(_, subId) ->
                { state with
                    subs = state.subs |> List.filter (fun x -> x.id <> subId) },
                []
            | _ -> state, []

module ParentWorker =
    open Elmish

    module S = SnapshotWorker'
    type State = { children: S.State list }
    type Msg = ChildMsg of int * S.Msg

    let update (state: State) (msg: Msg) =
        match msg with
        | ChildMsg(i, msg) ->
            let chState = state.children[i]
            let (chState, chCmds) = S.update chState msg

            let state' =
                { state with
                    children = state.children |> List.mapi (fun n x -> if i = n then chState else x) }

            state', mapCmd (fun x -> ChildMsg(i, x)) chCmds
