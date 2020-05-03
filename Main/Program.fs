open JsonFS.Parse
open JsonFS.Combinator

[<EntryPoint>]
let main argv =
    let stream = Stream("a123")
    let p = anyChar |>> System.Char.ToUpper
    printfn "%A" (p stream)
    0
