open JsonFS.Parse
open JsonFS.Combinator
open JsonFS.CharParsers

[<EntryPoint>]
let main argv =
    let stream = Stream(" 123")
    let p = ws >>. (many1 digit)
    printfn "%A" (stream |> p)
    0
