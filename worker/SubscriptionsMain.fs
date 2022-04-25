module Spectator.Worker.SubscriptionsMain

open System
open Spectator.Core

type State =
    { newSubscriptions: NewSubscription list }
    static member Empty = { newSubscriptions = [] }

module Domain =
    let removeNewSubs (subscriptions: NewSubscription list) sids =
        subscriptions
        |> List.filter (fun s -> not <| List.contains s.id sids)

    let update state (event:Event) =
        match event with
        | :? SubscriptionRemoved as SubscriptionRemoved (sids, nsids) ->
            { state with
                  newSubscriptions = removeNewSubs state.newSubscriptions nsids }
        | :? NewSubscriptionCreated as (NewSubscriptionCreated ns) ->
            { state with
                  newSubscriptions = ns :: state.newSubscriptions }
        | _ -> state

    let mkSubscriptionsEnd (state: State) (results: list<(PluginId * Uri) * Result<bool, _>>) : Event list =
        let findPlugin (ns: NewSubscription) : PluginId option =
            results
            |> List.tryPick
                (fun ((p, uri), r) ->
                    match r with
                    | Ok _ when uri = ns.uri -> Some p
                    | _ -> None)

        let mkSubscription (newSub: NewSubscription) (p: PluginId) : Subscription =
            { id = TypedId.wrap <| Guid.NewGuid()
              userId = newSub.userId
              provider = p
              uri = newSub.uri
              filter = newSub.filter }

        state.newSubscriptions
        |> List.collect
           @@ fun ns ->
               findPlugin ns
               |> Option.map
                  (fun pid ->
                      [ (mkSubscription ns pid |> SubscriptionCreated) :> Event
                        SubscriptionRemoved([], [ ns.id ]) ] )
               |> Option.defaultValue []

    let mkSubscription (parserIds: PluginId list) state =
        let mkRequest (ns: NewSubscription) =
            parserIds |> List.map @@ fun id -> id, ns.uri

        state.newSubscriptions
        |> List.map mkRequest
        |> List.concat

let restore = Domain.update

let main parserIds loadSubscriptions (reduce: IReducer<State, Event>) =
    async {
        let! reqs = reduce.Invoke(fun db -> db, [], Domain.mkSubscription parserIds db)

        let! responses =
            reqs
            |> List.map loadSubscriptions
            |> Async.Sequential
            |> Async.map @@ fun x -> Seq.zip reqs x |> Seq.toList

        do! reduce.Invoke(fun db -> db, Domain.mkSubscriptionsEnd db responses, ())
    }
