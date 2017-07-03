namespace Spectator.Core

open System

type Requests = NewSubscriptions

type UserId = Guid
type SubscriptionId = Guid

type Snapshot = {
    title: string
    url: string
}

type Subscription = {
    url: string
    snapshots: Snapshot list
}

type SubscriptionRequest = {
    id: SubscriptionId
    url: Uri
}

type SubscriptionType = Rss | WebPage

type CreateSubscriptionCommand = {
    id: SubscriptionId
    provider: SubscriptionType
}

type User = {
    id: UserId
    email: string
    passcode: string
    subscriptions: SubscriptionRequest list
}

module Auth =
    let register (email: string) (seed: string) (id: Guid) =
        let md5 = System.Security.Cryptography.MD5.Create()
        let passcode = seed + email
                       |> System.Text.Encoding.UTF8.GetBytes
                       |> md5.ComputeHash
                       |> System.Convert.ToBase64String
        { id = id
          email = email
          passcode = passcode
          subscriptions = [] }