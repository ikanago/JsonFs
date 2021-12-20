module Test.Array

open NUnit.Framework
open JsonFS.Parse
open JsonFS.JsonValue
open JsonFS.ParseJson
open Test.Common

let ParseArrayData =
    [
        "[]", (JArray [], Stream "")
        "[1, 2, 3]", (JArray [ JNumber 1m; JNumber 2m; JNumber 3m ], Stream "")
        "[1, 2,3]", (JArray [ JNumber 1m; JNumber 2m; JNumber 3m ], Stream "")
    ] |> List.map(PrepareTestCaseData)

[<TestCaseSource(nameof ParseArrayData)>]
let ParseArray s = Stream s |> parseArray

let ParseArrayFailData =
    [
        "[1, 2 ]", "Unexpected Token"
        "[1 , 2]", "Unexpected Token"
    ] |> List.map(PrepareFailTestCaseData)

[<TestCaseSource(nameof ParseArrayFailData)>]
let ParseArrayFail s = Stream s |> parseArray |> getExpectedException


