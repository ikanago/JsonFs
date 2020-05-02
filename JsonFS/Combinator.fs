// namespace JsonFS
namespace JsonFS

open System
open JsonFS.Parse

module Combinator =
    let satisfy f =
        fun (stream: Stream) ->
            match stream.Peek() with
            | Ok c ->
                if f c then
                    stream.Skip() |> ignore
                    Success(c, stream)
                else
                    Failure "Unexpected Token"
            | Error e -> Failure "EOF"

    let anyChar (stream: Stream) = satisfy (fun _ -> true) stream

    let digit (stream: Stream) = satisfy Char.IsDigit stream
