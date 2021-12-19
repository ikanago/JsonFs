module Test.Numbers

open NUnit.Framework
open JsonFS.Parse
open JsonFS.JsonValue
open JsonFS.ParseJson
open Test.Common

let ParseIntegerData = 
    [
        "0", JNumber 0m
        "123", JNumber 123m
    ] |> List.map(PrepareTestCaseData)

[<TestCaseSource(nameof ParseIntegerData)>]
let ParseInteger s = Stream s |> parseInteger