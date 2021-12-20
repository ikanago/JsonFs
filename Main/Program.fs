open JsonFS.Parse
open JsonFS.Combinator
open JsonFS.CharParsers

[<EntryPoint>]
let main argv =
    printfn "%A" (Stream "  a" |> (ws >>. specificChar 'a'))
    0
