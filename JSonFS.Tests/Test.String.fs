module Test.String

open NUnit.Framework
open JsonFS.Parse
open JsonFS.JsonValue
open JsonFS.ParseJson
open Test.Common

let ParseStringData =
    [
        "\"abc123\": xxx", (JString "abc123", Stream ": xxx")
    ] |> List.map(PrepareTestCaseData)

[<TestCaseSource(nameof ParseStringData)>]
let ParseString s = Stream s |> parseString

let ParseStringFailData =
    [
        "abc123", "Unexpected Token: a"
    ] |> List.map(PrepareFailTestCaseData)

[<TestCaseSource(nameof ParseStringFailData)>]
let ParseStringFail s = Stream s |> parseString |> getExpectedException

