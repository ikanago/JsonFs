namespace Test

open NUnit.Framework
open JsonFS.Parse

module Common =
    let PrepareTestCaseData (data: string, result) = TestCaseData(data).Returns(Success result)

    let PrepareFailTestCaseData (data: string, error: string) = TestCaseData(data).Returns(error)

    // Get a message of an exception in the `ParseResult`.
    // If `Success _` is passed, raise an exception.
    let getExpectedException (result: ParseResult<'T>) =
        match result with
        | Success (result, _) ->
            printfn "%A" result
            Assert.Fail()
            ""
        | Failure failure -> failure
