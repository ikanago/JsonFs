namespace JsonFS

open JsonFS.Parse
open JsonFS.Combinator

module CharParsers =
    // Read an any kind of character.
    let anyChar =
        fun (stream: Stream) ->
            match stream.Consume() with
            | Ok c -> Success (c, stream)
            | Error _ -> Failure "EOF"

    let lookAhead =
        fun (stream: Stream) ->
            match stream.Peek() with
            | Ok c -> Success (c, stream)
            | Error _ -> Failure "EOF"

    let satisfy pred =
        parser {
            let! c = lookAhead
            if pred c then
                do! consume
                return c
            else
                return! fail "Unexpected Token"
        }

    // Expect a specific character and read it.
    let specificChar (c: char) =
        fun (stream: Stream) -> satisfy ((=) c) stream

    let digit =
        fun (stream: Stream) -> satisfy System.Char.IsDigit stream

    let ws =
        let whitespaceCharacters = [ '\t'; '\n'; ' ' ]
        let isWhiteSpace = fun c -> List.exists (fun elem -> elem = c) whitespaceCharacters
        fun (stream: Stream) ->
            many (satisfy isWhiteSpace) stream
