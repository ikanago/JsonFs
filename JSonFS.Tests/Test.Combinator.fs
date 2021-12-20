module Test.Combinator

open NUnit.Framework
open JsonFS.Parse
open JsonFS.Combinator
open JsonFS.CharParsers

// Get a message of an exception in the `ParseResult`.
// If `Success _` is passed, raise an exception.
let getExpectedException (result: ParseResult<'T>) =
    match result with
    | Success (result, _) ->
        printfn "%A" result
        Assert.Fail()
        ""
    | Failure failure -> failure

[<Test>]
let andThenTest () =
    let stream = Stream "ab1c"

    let p =
        specificChar 'a' .>>. specificChar 'b' .>>. digit

    Assert.AreEqual(Success((('a', 'b'), '1'), Stream "c"), stream |> p)
    Assert.AreEqual("Unexpected Token", stream |> p |> getExpectedException)

[<Test>]
let orElseTest () =
    let stream = Stream "ab1c"

    let p =
        digit <|> specificChar 'a' <|> specificChar 'b'

    Assert.AreEqual(Success ('a', Stream "b1c"), stream |> p)
    Assert.AreEqual(Success ('b', Stream "1c"), stream |> p)
    Assert.AreEqual(Success ('1', Stream "c"), stream |> p)
    Assert.AreEqual("Unexpected Token", stream |> p |> getExpectedException)

[<Test>]
let fmapTest () =
    let stream = Stream "a1"
    let p = specificChar 'a' |>> System.Char.ToUpper
    Assert.AreEqual(Success ('A', Stream "1"), stream |> p)
    Assert.AreEqual("Unexpected Token", stream |> p |> getExpectedException)

[<Test>]
let andThenSelect () =
    let digitSemicolon = many1 digit .>> specificChar ';'
    let signDigit = opt (specificChar '+' <|> specificChar '-') >>. many1 digit
    Assert.AreEqual(Success (['1'; '2'; '3'], Stream ""), "123;" |> Stream |> digitSemicolon)
    Assert.AreEqual(Success (['1'; '2'; '3'], Stream ";"), "123;" |> Stream |> signDigit)
    Assert.AreEqual(Success (['1'; '2'; '3'], Stream ""), "+123" |> Stream |> signDigit)
    Assert.AreEqual(Success (['1'; '2'; '3'], Stream ""), "-123" |> Stream |> signDigit)

[<Test>]
let betweenTest () =
    let parens = between (specificChar '(') (specificChar ')') (many1 digit)
    Assert.AreEqual(Success (['1'; '2'; '3'], Stream ""), "(123)" |> Stream |> parens)
    Assert.AreEqual("EOF", "(123" |> Stream |> parens |> getExpectedException)

[<Test>]
let sequenceTest () =
    let stream = Stream "12a3b"

    let p1 =
        sequence [ digit; digit; specificChar 'a' ]
    Assert.AreEqual(Success ([ '1'; '2'; 'a' ], Stream "3b"), stream |> p1)

    let p2 = sequence [ digit; specificChar 'a' ]
    Assert.AreEqual("Unexpected Token", stream |> p2 |> getExpectedException)

[<Test>]
let manyTest () =
    let manyA = many (specificChar 'a')
    Assert.AreEqual(Success ([ 'a'; 'a'; 'a' ], Stream "b"), "aaab" |> Stream |> manyA)
    let many1A = many1 (specificChar 'a')
    Assert.AreEqual(Success ([ 'a'; 'a'; 'a' ], Stream "b"), "aaab" |> Stream |> many1A)
    Assert.AreEqual("Unexpected Token", "bbbb" |> Stream |> many1A |> getExpectedException)

[<Test>]
let optTest () =
    let p = opt (specificChar '-') .>>. many1 digit
    Assert.AreEqual(Success ((Some '-', [ '1'; '2'; '3' ]), Stream ""), "-123" |> Stream |> p)
    Assert.AreEqual(Success([ '1'; '2'; '3' ], Stream ""), "123" |> Stream |> (returnP snd <*> p))
