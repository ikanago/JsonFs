open JsonFS.Parse
open JsonFS.Combinator

[<EntryPoint>]
let main argv =
    let stream = Stream("b12")
    let p = digit <|> specificChar 'a'
    printfn "%A" (p stream)
    0
