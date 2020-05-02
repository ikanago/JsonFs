module JSonFS.Tests

open NUnit.Framework
open JsonFS.Parse

exception UnExpectedConditionException of string

// Get value in the `ParseResult`
let getResult (result: JsonFS.Parse.ParseResult) =
    match result with
    | Success (c, _) -> string c
    | Failure failure -> failure

[<Test>]
let anyCharTest () =
    let stream = Stream "n"
    Assert.AreEqual("n", stream |> anyChar |> getResult)
    Assert.AreEqual("EOF", stream |> anyChar |> getResult)

[<Test>]
let StreamTest () =
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

[<Test>]
let digitTest () =
    let stream = Stream "1a"
    Assert.AreEqual("1", stream |> digit |> getResult)
    Assert.AreEqual("Unexpected Token", stream |> digit |> getResult)
