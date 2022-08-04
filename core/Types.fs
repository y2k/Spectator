namespace Spectator.Core

open System

type UserId = string
type PluginId = Guid

[<MeasureAnnotatedAbbreviation>]
type 'a TypedId = Guid

#nowarn "42"

module TypedId =
    let inline wrap<'b> (a: Guid) : 'b TypedId = (# "" a : TypedId<'b> #)
    let empty () = wrap Guid.Empty
    let inline unwrap (x: _ TypedId) = (# "" x : Guid #)

[<CLIMutable>]
type Subscription =
    { id: Subscription TypedId
      userId: UserId
      provider: PluginId
      uri: Uri
      filter: string }
    static member empty =
        { id = TypedId.wrap Guid.Empty
          userId = ""
          provider = Guid.Empty
          uri = null
          filter = "" }

[<CLIMutable>]
type NewSubscription =
    { id: NewSubscription TypedId
      userId: UserId
      uri: Uri
      filter: string }
    static member empty =
        { id = TypedId.wrap Guid.Empty
          userId = ""
          uri = null
          filter = "" }

[<CLIMutable>]
type Snapshot =
    { subscriptionId: Subscription TypedId
      created: DateTime
      id: Snapshot TypedId
      title: string
      uri: Uri }
    static member empty =
        { subscriptionId = TypedId.wrap Guid.Empty
          created = DateTime.MinValue
          id = TypedId.wrap Guid.Empty
          title = ""
          uri = null }

type Event =
    interface
    end

type Command = Event
type SyntheticEvent = Event

type NewSubscriptionCreated =
    | NewSubscriptionCreated of NewSubscription
    interface SyntheticEvent

type SubscriptionCreated =
    | SubscriptionCreated of Subscription
    interface Command

type SnapshotCreated =
    | SnapshotCreated of isNew: bool * Snapshot
    interface SyntheticEvent

type SubscriptionRemoved =
    | SubscriptionRemoved of Subscription TypedId list * NewSubscription TypedId list
    interface SyntheticEvent

type Filter =
    | NoneFilter
    | UserFilter of string

type TelegramMessageReceived =
    | TelegramMessageReceived of user: string * message: string
    interface Event

type SendTelegramMessage =
    | SendTelegramMessage of user: string * message: string
    interface Command

type TimerTicked =
    | TimerTicked of minNumber: int64
    interface Event

type DownloadHttp =
    | DownloadHttp of Uri * (Result<byte [], exn> -> Event)
    interface Command

module StoreAtom =
    type 's StateStore = { mutable state: 's }

    let inline make () : ^state StateStore =
        { state = (^state: (static member empty: ^state) ()) }

    let addStateCofx (state: _ StateStore) f = fun x -> f state.state x

    let handleCommand (stateHolder: 'state StateStore) (cmd: Command) =
        match cmd with
        | :? 'state as newState -> stateHolder.state <- newState
        | _ -> ()

    let handleCommandFun (stateHolder: 'state StateStore) update (cmd: #Command) =
        stateHolder.state <- update stateHolder.state cmd
