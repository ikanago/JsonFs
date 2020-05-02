module Test.Combinator

open NUnit.Framework
open JsonFS.Parse
open JsonFS.Combinator

// Exception for `getExpectException`
exception UnexpectedSuccessException of string

// Get a message of an exception in the `ParseResult`.
// If `Success _` is passed, raise an exception.
let getExpectException (result: ParseResult<'T>) =
    match result with
    | Success _ -> raise (UnexpectedSuccessException("Unreachable"))
    | Failure failure -> failure

[<Test>]
let anyCharTest () =
    let stream = Stream "a"
    Assert.AreEqual(Success 'a', stream |> anyChar)
    Assert.AreEqual("EOF", stream |> anyChar |> getExpectException)

[<Test>]
let specificCharTest () =
    let stream = Stream "ab"
    Assert.AreEqual(Success 'a', stream |> specificChar 'a')
    Assert.AreEqual("Unexpected Token", stream |> specificChar 'a' |> getExpectException)

[<Test>]
let digitTest () =
    let stream = Stream "1a"
    Assert.AreEqual(Success '1', stream |> digit)
    Assert.AreEqual("Unexpected Token", stream |> digit |> getExpectException)
