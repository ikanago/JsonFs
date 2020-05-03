module Test.Combinator

open NUnit.Framework
open JsonFS.Parse
open JsonFS.Combinator

// Exception for `getExpectException`
exception UnexpectedSuccessException of string

// Get a message of an exception in the `ParseResult`.
// If `Success _` is passed, raise an exception.
let getExpectedException (result: ParseResult<'T>) =
    match result with
    | Success _ -> raise (UnexpectedSuccessException("Unreachable"))
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
let orElseTest () =
    let stream = Stream "ab1c"
    let p = digit <|> specificChar 'a' <|> specificChar 'b'
    Assert.AreEqual(Success 'a', stream |> p)
    Assert.AreEqual(Success 'b', stream |> p)
    Assert.AreEqual(Success '1', stream |> p)
    Assert.AreEqual("Unexpected Token", stream |> p |> getExpectedException)
