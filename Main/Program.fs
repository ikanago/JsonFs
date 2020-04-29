open JsonFS

[<EntryPoint>]
let main argv =
    printfn "%A" JsonValue.object
    0 // return an integer exit code
