module Spectator.Config

type Config =
    { filesDir: string
      mongoDomain: string
      restTelegramPassword: string
      restTelegramBaseUrl: string
      telegramToken: string
      updateTimeMinutes: int }

let readConfig path: Config =
    System.IO.File.ReadAllText path
    |> Legivel.Serialization.Deserialize
    |> function
    | [ Legivel.Serialization.Succes { Legivel.Serialization.Data = x } ] -> x
    | e -> failwithf "Can't parse config: %O" e
