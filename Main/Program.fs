open JsonFS.Parse
open JsonFS.Combinator

[<EntryPoint>]
let main argv =
    let stream = Stream("aaa123a")
    let p = many1 (specificChar 'a')
    printfn "%A" (p stream)
    0
