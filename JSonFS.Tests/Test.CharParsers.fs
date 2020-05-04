module Test.CharParsers

open NUnit.Framework
open JsonFS.Parse
open JsonFS.Combinator
open JsonFS.CharParsers
open Test.Combinator

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

[<Test>]
let sepByTest () =
    let numArray = between (specificChar '[') (specificChar ']') (sepBy1 digit (specificChar ',' .>>. ws))
    Assert.AreEqual(Success(['1'; '2'; '3']), "[1, 2,3]" |> Stream |> numArray)
    // Assert.AreEqual("Unexpected Token", "1,2,3" |> Stream |> (sepBy1 digit (specificChar ',') .>>. specificChar 'a') |> getExpectedException)
