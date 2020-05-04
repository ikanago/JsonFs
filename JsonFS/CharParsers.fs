namespace JsonFS

open JsonFS.Parse
open JsonFS.Combinator

module CharParsers =
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

    let ws =
        let whitespaceCharacters = [ '\t'; '\n'; ' ' ]
        fun (stream: Stream) ->
            many (satisfy (fun c -> List.exists (fun elem -> elem = c) whitespaceCharacters)) stream
