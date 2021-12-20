module Test.Null

open NUnit.Framework
open JsonFS.Parse
open JsonFS.JsonValue
open JsonFS.ParseJson
open Test.Common

let ParseNullData =
    [
        "null", (JNull , Stream "")
    ] |> List.map(PrepareTestCaseData)

[<TestCaseSource(nameof ParseNullData)>]
let ParseNull s = Stream s |> parseNull
