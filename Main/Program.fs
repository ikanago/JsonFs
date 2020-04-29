open JsonFS

[<EntryPoint>]
let main argv =
    printfn "%A" (Parse.anyChar "hoge")
    0 // return an integer exit code
