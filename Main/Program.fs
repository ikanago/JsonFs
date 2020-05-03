open JsonFS.Parse
open JsonFS.Combinator

[<EntryPoint>]
let main argv =
    let stream = Stream("123")
    let p = sequence [digit; digit; digit]
    printfn "%A" (p stream)
    0
