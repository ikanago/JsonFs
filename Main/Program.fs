open JsonFS.Parse
open JsonFS.Combinator

[<EntryPoint>]
let main argv =
    let stream = Stream("-123")
    let p = (specificChar '+' <|> specificChar '-') >>. many1 digit
    printfn "%A" (stream |> p)
    0
