open JsonFS.Parse

[<EntryPoint>]
let main argv =
    let stream = Stream("")
    printfn "%A" (anyChar stream)
    0
