open JsonFS.Parse
open JsonFS.Combinator

[<EntryPoint>]
let main argv =
    let stream = Stream("-123a")
    let p = opt (specificChar '-') .>>. digit
    printfn "%A" (p stream)
    0
