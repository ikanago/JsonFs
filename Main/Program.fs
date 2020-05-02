open JsonFS.Parse
open JsonFS.Combinator

[<EntryPoint>]
let main argv =
    let stream = Stream("a12")
    printfn "%A" (digit stream)
    0
