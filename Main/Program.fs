open JsonFS.Parse
open JsonFS.Combinator
open JsonFS.CharParsers
open JsonFS.ParseJson

[<EntryPoint>]
let main argv =
    let v = Stream "[1]" |> (between (specificChar '[') (specificChar ']') digit)
    printfn "%A" v
    0
