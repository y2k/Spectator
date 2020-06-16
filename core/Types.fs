namespace Spectator.Core

open System

type UserId = string
type PluginId = Guid

[<MeasureAnnotatedAbbreviation>]
type 'a TypedId = Guid
#nowarn "42"
module TypedId =
    let inline wrap<'b> (a : Guid) : 'b TypedId = (# "" a : TypedId<'b> #)
    let empty () = wrap Guid.Empty

[<CLIMutable>]
type Subscription =
    { id : Subscription TypedId
      userId : UserId
      provider : PluginId
      uri : Uri
      filter : string }
    static member empty = { id = TypedId.wrap Guid.Empty; userId = ""; provider = Guid.Empty; uri = null; filter = "" }

[<CLIMutable>]
type NewSubscription =
    { id : NewSubscription TypedId
      userId : UserId
      uri : Uri
      filter : string }
      static member empty = { id = TypedId.wrap Guid.Empty; userId = ""; uri = null; filter = "" }

[<CLIMutable>]
type Snapshot =
    { subscriptionId : Subscription TypedId
      created : DateTime
      id : Snapshot TypedId
      title : string
      uri : Uri }
    static member empty = { subscriptionId = TypedId.wrap Guid.Empty; created = DateTime.MinValue; id = TypedId.wrap Guid.Empty; title = ""; uri = null }

type Events =
    | NewSubscriptionCreated of NewSubscription
    | SubscriptionCreated of Subscription
    | SubscriptionRemoved of Subscription TypedId list * NewSubscription TypedId list
    | SnapshotCreated of Snapshot

type Filter = NoneFilter | UserFilter of string
