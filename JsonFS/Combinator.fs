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

    // First, apply the parser `p`.
    // If `p` succeeds, return the result of `p`.
    // If `p` fails, apply the parser `q` and return the result of `q`.
    let (<|>) (p: Parser<'a>) (q: Parser<'a>) =
        fun (stream: Stream) ->
            let pResult = p stream
            match pResult with
            | Success _ -> pResult
            | Failure _ -> q stream
