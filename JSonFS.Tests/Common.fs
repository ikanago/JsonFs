namespace Test

open NUnit.Framework
open JsonFS.Parse

module Common =
    let PrepareTestCaseData (data: string, result) =  TestCaseData(data).Returns(Success result)
