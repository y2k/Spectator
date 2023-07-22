open System

// Guid.NewGuid() |> printfn "%O"

// b77cd782-0d84-4946-8dfd-b684eaff3f82

let random = Random(42)

let a: int array = [| random.Next(); random.Next(); random.Next(); random.Next() |]

let b: byte array = Array.create 16 0uy

Buffer.BlockCopy(a, 0, b, 0, 16)

Guid b |> printfn "%O"
