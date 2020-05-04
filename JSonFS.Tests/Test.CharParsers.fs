module Test.CharParsers

open NUnit.Framework
open JsonFS.Parse
open JsonFS.Combinator
open JsonFS.CharParsers

// Get a message of an exception in the `ParseResult`.
// If `Success _` is passed, raise an exception.
let getExpectedException (result: ParseResult<'T>) =
    match result with
    | Success result ->
        printfn "%A" result
        Assert.Fail()
        ""
    | Failure failure -> failure

[<Test>]
let anyCharTest () =
    let stream = Stream "a"
    Assert.AreEqual(Success 'a', stream |> anyChar)
    Assert.AreEqual("EOF", stream |> anyChar |> getExpectedException)

[<Test>]
let specificCharTest () =
    let stream = Stream "ab"
    Assert.AreEqual(Success 'a', stream |> specificChar 'a')
    Assert.AreEqual("Unexpected Token", stream |> specificChar 'a' |> getExpectedException)

[<Test>]
let digitTest () =
    let stream = Stream "1a"
    Assert.AreEqual(Success '1', stream |> digit)
    Assert.AreEqual("Unexpected Token", stream |> digit |> getExpectedException)

[<Test>]
let wsTest () =
    let ignoreWs = ws >>. specificChar 'a'
    Assert.AreEqual(Success 'a', " \t\n\n   a" |> Stream |> ignoreWs)
