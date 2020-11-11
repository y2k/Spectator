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

module Persistent =
    type IInsert =
        abstract invoke: string -> 'a -> unit Async

    type IForEach =
        abstract invoke: string -> ('a -> unit) -> unit Async

    // let applyObj (forEach: IForEach) update =
    //     async {
    //         do! forEach.invoke "subscriptions" (fun (s: Subscription) -> update <| SubscriptionCreated s)
    //         do! forEach.invoke "snapshots" (fun (s: Snapshot) -> update <| SnapshotCreated(false, s))
    //     }

    let restoreState applyObj (forEach: IForEach) emptyState f =
        async {
            let state = ref emptyState
            let update e = state := f !state e

            do! applyObj forEach update

            // do! forEach.invoke "subscriptions" (fun (s: Subscription) -> update <| SubscriptionCreated s)
            // do! forEach.invoke "snapshots" (fun (s: Snapshot) -> update <| SnapshotCreated(false, s))

            return !state
        }

    type 'e State = { queue: 'e list }

    let initState = { queue = [] }

    let restore s e = { queue = e :: s.queue }

    // let applyEvent (insert: IInsert) delete e =
    //     async {
    //         match e with
    //         | SubscriptionCreated sub -> do! insert.invoke "subscriptions" sub
    //         | SubscriptionRemoved (sids, _) ->
    //             for id in sids do
    //                 let id: System.Guid = TypedId.unwrap id
    //                 do! delete "subscriptions" id
    //         | SnapshotCreated (_, snap) -> do! insert.invoke "snapshots" snap
    //         | NewSubscriptionCreated _
    //         | HealthCheckRequested _ -> ()
    //     }

    let main applyEvent (insert: IInsert) delete reducer =
        async {
            let! db = reducer (fun db -> { queue = [] }, [])

            for e in db.queue |> List.rev do
                do! applyEvent insert delete e

            do! Async.Sleep 1_000
        }
