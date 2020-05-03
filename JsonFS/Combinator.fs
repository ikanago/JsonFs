namespace JsonFS

open JsonFS.Parse

type Parser<'a> = Stream -> ParseResult<'a>

module Combinator =
    let satisfy f =
        fun (stream: Stream) ->
            match stream.Peek() with
            | Ok c ->
                if f c then
                    stream.Skip() |> ignore
                    Success c
                else
                    Failure "Unexpected Token"
            | Error e -> Failure "EOF"

    // Read an any kind of character.
    let anyChar =
        fun (stream: Stream) -> satisfy (fun _ -> true) stream

    // Expect a specific character and read it.
    let specificChar (c: char) =
        fun (stream: Stream) -> satisfy ((=) c) stream

    let digit =
        fun (stream: Stream) -> satisfy System.Char.IsDigit stream

    // Apply both parser `p` and `q`.
    // If both of them succeed, return results wrapped into tuple.
    // If one of them fails, report the failure.
    let (.>>.) (p: Parser<'a>) (q: Parser<'b>) =
        fun (stream: Stream) ->
            let pResult = p stream
            match pResult with
            | Failure failure -> Failure failure
            | Success pValue ->
                let qResult = q stream
                match qResult with
                | Failure failure -> Failure failure
                | Success qValue -> Success(pValue, qValue)

    // First, apply the parser `p`.
    // If `p` succeeds, return the result of `p`.
    // If `p` fails, apply the parser `q` and return the result of `q`.
    let (<|>) (p: Parser<'a>) (q: Parser<'a>) =
        fun (stream: Stream) ->
            let pResult = p stream
            match pResult with
            | Success _ -> pResult
            | Failure _ -> q stream

    let (|>>) (p: Parser<'a>) f =
        fun (stream: Stream) ->
            let result = p stream
            match result with
            | Failure failure -> Failure failure
            | Success value -> Success (f value)
