module Test.Combinator

open NUnit.Framework
open JsonFS.Parse
open JsonFS.Combinator
open JsonFS.CharParsers
open Test.Common

[<Test>]
let andThenTest () =
    let stream = Stream "ab1c"

    let p =
        specificChar 'a' .>>. specificChar 'b' .>>. digit

    Assert.AreEqual(Success((('a', 'b'), '1'), Stream "c"), stream |> p)
    Assert.AreEqual("Unexpected Token: c", stream |> p |> getExpectedException)

[<Test>]
let orElseTest () =
    let p =
        digit <|> specificChar 'a' <|> specificChar 'b'

    Assert.AreEqual(Success ('a', Stream "b1c"), Stream "ab1c" |> p)
    Assert.AreEqual(Success ('b', Stream "1c"), Stream "b1c" |> p)
    Assert.AreEqual(Success ('1', Stream "c"), Stream "1c" |> p)
    Assert.AreEqual("Unexpected Token: c", Stream "c" |> p |> getExpectedException)

[<Test>]
let fmapTest () =
    let stream = Stream "a1"
    let p = specificChar 'a' |>> System.Char.ToUpper
    Assert.AreEqual(Success ('A', Stream "1"), stream |> p)
    Assert.AreEqual("Unexpected Token: 1", stream |> p |> getExpectedException)

[<Test>]
let andThenSelect () =
    let digitSemicolon = some digit .>> specificChar ';'
    let signDigit = opt (specificChar '+' <|> specificChar '-') >>. some digit
    Assert.AreEqual(Success (['1'; '2'; '3'], Stream ""), "123;" |> Stream |> digitSemicolon)
    Assert.AreEqual(Success (['1'; '2'; '3'], Stream ";"), "123;" |> Stream |> signDigit)
    Assert.AreEqual(Success (['1'; '2'; '3'], Stream ""), "+123" |> Stream |> signDigit)
    Assert.AreEqual(Success (['1'; '2'; '3'], Stream ""), "-123" |> Stream |> signDigit)

[<Test>]
let betweenTest () =
    let parens = between (specificChar '(') (specificChar ')') (some digit)
    Assert.AreEqual(Success (['1'; '2'; '3'], Stream ""), "(123)" |> Stream |> parens)
    Assert.AreEqual("EOF", "(123" |> Stream |> parens |> getExpectedException)

[<Test>]
let sequenceTest () =
    let stream = Stream "12a3b"

    let p1 =
        sequence [ digit; digit; specificChar 'a' ]
    Assert.AreEqual(Success ([ '1'; '2'; 'a' ], Stream "3b"), stream |> p1)

    let p2 = sequence [ digit; specificChar 'a' ]
    Assert.AreEqual("Unexpected Token: b", stream |> p2 |> getExpectedException)

[<Test>]
let manyTest () =
    let manyA = many (specificChar 'a')
    Assert.AreEqual(Success ([ 'a'; 'a'; 'a' ], Stream "b"), "aaab" |> Stream |> manyA)
    let someA = some (specificChar 'a')
    Assert.AreEqual(Success ([ 'a'; 'a'; 'a' ], Stream "b"), "aaab" |> Stream |> someA)
    Assert.AreEqual("Unexpected Token", "bbbb" |> Stream |> someA |> getExpectedException)

[<Test>]
let tryTest () =
    let digitOrA = digit <|> specificChar 'A'
    Assert.AreEqual(Success ('1', Stream "A"), Stream "1A" |> digitOrA)
    Assert.AreEqual(Success ('A', Stream "A"), Stream "AA" |> digitOrA)

[<Test>]
let optTest () =
    let p = opt (specificChar '-') .>>. some digit
    Assert.AreEqual(Success ((Some '-', [ '1'; '2'; '3' ]), Stream ""), "-123" |> Stream |> p)
    Assert.AreEqual(Success([ '1'; '2'; '3' ], Stream ""), "123" |> Stream |> (returnP snd <*> p))

[<Test>]
let sepByTest () =
    let numArray = between (specificChar '[') (specificChar ']') (sepBy1 digit (specificChar ',' .>>. ws))
    Assert.AreEqual(Success (['1'; '2'; '3'], Stream ""), "[1, 2,3]" |> Stream |> numArray)
    Assert.AreEqual("EOF", "1,2,3" |> Stream |> (sepBy1 digit (specificChar ',') .>>. specificChar 'a') |> getExpectedException)
