namespace JsonFS

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

    type ParseResult<'T> =
        | Success of 'T
        | Failure of string
