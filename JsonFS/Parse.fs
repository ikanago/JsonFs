namespace JsonFS

open System

module Parse =
    type Stream(stream: string) as self =
        let stream = stream
        let mutable position = 0

        member private this.IsConsumable() = stream.Length > position

        member this.Peek() = if self.IsConsumable() then Ok stream.[position] else Error "EOF"

        member this.Skip() =
            match self.Peek() with
            | Ok _ ->
                position <- position + 1
                Ok()
            | Error e -> Error e

    type ParseResult =
        | Success of char * Stream
        | Failure of string

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
