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

        member this.Consume() =
            match this.Peek() with
            | Ok c -> 
                this.Skip() |> ignore
                Ok c
            | e -> e

        member this.Backward() =
            if position <> 0 then
                position <- position - 1

        member this.Inner() = stream.[position..]

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

    type ParserBuilder() =
        member this.Bind(p, f) =
            fun (stream: Stream) ->
                match p stream with
                | Success (v, stream) -> f v stream
                | Failure e -> Failure e

        member this.Return(x) = fun (stream: Stream) -> Success (x, stream)

        member this.ReturnFrom(x) = x

        member this.Delay(f) = f()

        member this.Zero() = fun (_: Stream) -> Failure ""

        member this.Combine(p, q) = fun (stream: Stream) ->
            match p stream with
            | Success (v, stream) -> Success (v, stream)
            | Failure _ -> q stream

    let parser = ParserBuilder()

    let returnP x : Parser<'a> = fun (stream: Stream) -> Success (x, stream)

    let consume =
        fun (stream: Stream) ->
            match stream.Skip() with
            | Ok _ -> Success ((), stream)
            | Error _ -> Failure "EOF"

    let backward =
        fun (stream: Stream) ->
            stream.Backward()
            Success ((), stream)

    let fail (msg: string) = fun (_: Stream) -> Failure msg
