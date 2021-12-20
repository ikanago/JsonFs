namespace JsonFS

module Parse =
    type Stream(stream: string) as self =
        let stream = stream
        let mutable position: int = 0

        member private this.IsConsumable() = stream.Length > position

        member this.Position() = position

        member this.Peek() = if self.IsConsumable() then Ok stream.[position] else Error "EOF"

        member this.Consume() =
            match this.Peek() with
            | Ok c ->
                position <- position + 1
                Ok c
            | e -> e

        member this.Skip() =
            match this.Consume() with
            | Ok _ -> Ok ()
            | Error e -> Error e

        member this.BackTo(pos: int) =
            position <- max (position - pos) 0

        member this.Inner() = stream.[(position)..]

        override this.Equals other =
            match other with
            | :? Stream as other -> this.Inner().Equals(other.Inner())
            | _ -> false

        override this.GetHashCode() = this.Inner().GetHashCode()

        override this.ToString() = this.Inner().ToString()

    type ParseResult<'T> =
        | Success of 'T * Stream
        | Failure of string

    type Parser<'a> = Stream -> ParseResult<'a>

    let returnP x : Parser<'a> = fun (stream: Stream) -> Success (x, stream)

    let ignoreP: Parser<unit> = returnP ()

    let consume =
        fun (stream: Stream) ->
            match stream.Skip() with
            | Ok _ -> Success ((), stream)
            | Error _ -> Failure "EOF"

    let fail (msg: string) = fun (_: Stream) -> Failure msg

    type ParserBuilder() =
        member this.Bind(p, f) =
            fun (stream: Stream) ->
                match p stream with
                | Success (v, stream) -> f v stream
                | Failure e -> Failure e

        member this.Return(x) = returnP x

        member this.ReturnFrom(x) = x

        member this.Delay(f) = f()

        member this.Zero() = fun (_: Stream) -> Failure ""

        member this.Combine(p, q) = fun (stream: Stream) ->
            match p stream with
            | Success (v, stream) -> Success (v, stream)
            | Failure _ -> q stream

    let parser = ParserBuilder()
