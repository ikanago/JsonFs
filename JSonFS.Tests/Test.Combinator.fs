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
    | Success result ->
        printfn "%A" result
        raise (UnexpectedSuccessException("Unreachable"))
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
let andThenTest () =
    let stream = Stream "ab1c"
    let p = specificChar 'a' .>>. specificChar 'b' .>>. digit
    Assert.AreEqual(Success(('a', 'b'), '1'), stream |> p)
    Assert.AreEqual("Unexpected Token", stream |> p |> getExpectedException)

[<Test>]
let orElseTest () =
    let stream = Stream "ab1c"
    let p = digit <|> specificChar 'a' <|> specificChar 'b'
    Assert.AreEqual(Success 'a', stream |> p)
    Assert.AreEqual(Success 'b', stream |> p)
    Assert.AreEqual(Success '1', stream |> p)
    Assert.AreEqual("Unexpected Token", stream |> p |> getExpectedException)

[<Test>]
let fmapTest () =
    let stream = Stream "a1"
    let p = specificChar 'a' |>> System.Char.ToUpper
    Assert.AreEqual(Success 'A', stream |> p)
    Assert.AreEqual("Unexpected Token", stream |> p |> getExpectedException)

[<Test>]
let sequenceTest () =
    let stream = Stream "12a3b"
    let p1 = sequence [digit; digit; specificChar 'a']
    let p2 = sequence [digit; specificChar 'a']
    Assert.AreEqual(Success['1'; '2'; 'a';], stream |> p1)
    Assert.AreEqual("Unexpected Token", stream |> p2 |> getExpectedException)

[<Test>]
let manyTest() =
    let stream = Stream "aaab"
    let manyA = many (specificChar 'a')
    Assert.AreEqual(Success ['a'; 'a'; 'a'], stream |> manyA)
    let stream = Stream "aaab"
    let many1A = many1 (specificChar 'a')
    Assert.AreEqual(Success ['a'; 'a'; 'a'], stream |> many1A)
    let stream = Stream "bbbb"
    Assert.AreEqual("Unexpected Token", stream |> many1A |> getExpectedException)
