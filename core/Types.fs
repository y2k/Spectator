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

type Command =
    interface
    end

type NewSubscriptionCreated =
    | NewSubscriptionCreated of NewSubscription
    interface Command

type SubscriptionCreated =
    | SubscriptionCreated of Subscription
    interface Command

type SnapshotCreated =
    | SnapshotCreated of isNew: bool * Snapshot
    interface Command

type SubscriptionRemoved =
    | SubscriptionRemoved of Subscription TypedId list * NewSubscription TypedId list
    interface Command

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
    | TimerTicked of minuteNumber: int64
    interface Event

type DownloadHttp =
    | DownloadHttp of Uri * (Result<byte [], exn> -> Event)
    interface Command
