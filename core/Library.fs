namespace Spectator.Core

open System

// Types

type UserId = string
type SubscriptionId = Guid
type ProviderId = Guid

type NewSubscription = {
    userId: UserId
    uri: Uri
}

type Subscription = {
    id: SubscriptionId
    userId: UserId
    provider: ProviderId
    uri: Uri
}

type Snapshot = {
    subscriptionId: Guid
    title: string
    uri: Uri
}

// EasyNetQ Commands

type Command =
    | GetNewSubscriptions
    | CreateSubscriptions of (Uri * ProviderId) list
    | AddSubscription // TODO:
    | AddSnapshot // TODO:
    | AddNewSubscription of UserId * Uri
    | GetUserSubscriptions of UserId

type Responses =
    | NewSubscriptions of NewSubscription list
    | UserSubscriptions of NewSubscription list * Subscription list
    | Unit

module Auth =
    let computeAuthKey (user: UserId) (seed: string) =
        let md5 = System.Security.Cryptography.MD5.Create()
        seed + user
        |> System.Text.Encoding.UTF8.GetBytes
        |> md5.ComputeHash
        |> System.Convert.ToBase64String