module JSonFS.Tests

open NUnit.Framework
open JsonFS.Parse

[<Test>]
let ReadOneCharacter () =
    let expected = Some("n")
    let actual = anyChar "name"
    Assert.AreEqual(expected, actual)
