module Test.CharParsers

open NUnit.Framework
open JsonFS.Parse
open JsonFS.Combinator
open JsonFS.CharParsers
open Test.Common

[<Test>]
let anyCharTest () =
    let stream = Stream "a"
    Assert.AreEqual(Success ('a', Stream ""), stream |> anyChar)
    printfn "%A" stream
    Assert.AreEqual("EOF", stream |> anyChar |> getExpectedException)

[<Test>]
let specificCharTest () =
    let stream = Stream "ab"
    Assert.AreEqual(Success ('a', Stream "b"), stream |> specificChar 'a')
    Assert.AreEqual("Unexpected Token", stream |> specificChar 'a' |> getExpectedException)

[<Test>]
let digitTest () =
    let stream = Stream "1a"
    Assert.AreEqual(Success ('1', Stream "a"), stream |> digit)
    Assert.AreEqual("Unexpected Token", stream |> digit |> getExpectedException)

[<Test>]
let wsTest () =
    let ignoreWs = ws >>. specificChar 'a'
    Assert.AreEqual(Success ('a', Stream ""), " \t\n\n   a" |> Stream |> ignoreWs)
