module JSonFS.Tests

open NUnit.Framework
open JsonFS.Parse

let getResult (result: JsonFS.Parse.ParseResult) =
    match result with
    | Success (c, _) -> string c
    | Failure failure -> failure

[<Test>]
let ReadOneCharacter () =
    let expected = "n"
    let actual = "name" |> Stream |> anyChar |> getResult
    Assert.AreEqual(expected, actual)

    let expected = Failure "EOF"
    let actual = "" |> Stream |> anyChar
    Assert.AreEqual(expected, actual)

exception UnExpectedConditionException of string
[<Test>]
let TestStream () =
    let stream = Stream "ab"
    match stream.Peek() with
    | Ok c ->
        Assert.AreEqual('a', c)
        stream.Skip() |> ignore
    | Error _ -> ()

    match stream.Peek() with
    | Ok c ->
        Assert.AreEqual('b', c)
        stream.Skip() |> ignore
    | Error _ -> ()
    
    match stream.Peek() with
    | Error "EOF" -> ()
    | _ -> raise (UnExpectedConditionException("UnExpected"))
