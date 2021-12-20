module Test.Numbers

open NUnit.Framework
open JsonFS.Parse
open JsonFS.JsonValue
open JsonFS.ParseJson
open Test.Common

let ParseIntegerData =
    [
        "0", (JNumber 0m, Stream "")
        "123", (JNumber 123m, Stream "")
        "123a456", (JNumber 123m, Stream "a456")
        "-123", (JNumber -123m, Stream "")
        "-123a456", (JNumber -123m, Stream "a456")
        "-0", (JNumber 0m, Stream "")
    ] |> List.map(PrepareTestCaseData)

[<TestCaseSource(nameof ParseIntegerData)>]
let ParseInteger s = Stream s |> parseInteger
