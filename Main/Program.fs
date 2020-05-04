open JsonFS.Parse
open JsonFS.Combinator
open JsonFS.CharParsers

[<EntryPoint>]
let main argv =
    let stream = Stream(",")
    let p = (specificChar ',') .>>. digit
    printfn "%A" (stream |> p)
    printfn "%A" (stream.Peek())
    0
