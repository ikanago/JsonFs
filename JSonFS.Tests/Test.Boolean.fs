module Test.Boolean

open NUnit.Framework
open JsonFS.Parse
open JsonFS.JsonValue
open JsonFS.ParseJson
open Test.Common

let ParseBooleanData =
    [
        "true", (JBoolean true , Stream "")
        "false", (JBoolean false , Stream "")
    ] |> List.map(PrepareTestCaseData)

[<TestCaseSource(nameof ParseBooleanData)>]
let ParseBoolean s = Stream s |> parseBoolean
