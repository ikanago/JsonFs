namespace JsonFS

open JsonFS.Parse

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
    let anyChar = fun stream -> satisfy (fun _ -> true) stream

    // Expect a specific character and read it.
    let specificChar c = fun stream -> satisfy ((=) c) stream

    let digit = fun stream -> satisfy System.Char.IsDigit stream
