namespace JsonFS

open JsonFS.Parse

module Combinator =
    // Apply both parser `p` and `q`.
    // If both of them succeed, return results wrapped into tuple.
    // If one of them fails, report the failure.
    let (.>>.) (p: Parser<'a>) (q: Parser<'b>) =
        parser {
            let! pResult = p
            let! qResult = q
            return (pResult, qResult)
        }

    // First, apply the parser `p`.
    // If `p` succeeds, return the result of `p`.
    // If `p` fails, apply the parser `q` and return the result of `q`.
    let (<|>) (p: Parser<'a>) (q: Parser<'a>): Parser<'a> =
        parser {
            return! p
            return! q
        }

    // Functor
    //
    // First, apply the parser `p`.
    // If `p` succeeds, then apply f to the result of `p` and
    // return it as `ParseResult<'a>`.
    let (|>>) (p: Parser<'a>) (f: 'a -> 'b): Parser<'b> =
        fun (stream: Stream) ->
            let result = p stream
            match result with
            | Failure failure -> Failure failure
            | Success (v, stream) -> Success((f v, stream))

    // Applicative
    let (<*>) (fP: Parser<'a -> 'b>) (xP: Parser<'a>): Parser<'b> = (fP .>>. xP) |>> (fun (f, x) -> f x)

    let lift2 f xP yP = (returnP f <*> xP <*> yP)

    // Apply parsers `p` and `q` in sequence and throw away the result of `q`.
    let (.>>) (p: Parser<'a>) (q: Parser<'b>) = lift2 (fun a _ -> a) p q

    // Apply parsers `p` and `q` in sequence and throw away the result of `p`.
    let (>>.) (p: Parser<'a>) (q: Parser<'b>) = lift2 (fun _ b -> b) p q

    // Used to parse something surrounded by specific string parsed by `popen` and `pclose`.
    // For example, "{abc}".
    let between (popen: Parser<'a>) (pclose: Parser<'b>) (p: Parser<'c>) = popen >>. p .>> pclose

    // Apply parsers in `pList`.
    let rec sequence (pList: list<Parser<'a>>) =
        let cons head tail = head :: tail
        match pList with
        | [] -> returnP []
        | headP :: tailP -> (lift2 cons) headP (sequence tailP)

    // Parse greedy as long as possible.
    let many (p: Parser<'a>): Parser<list<'a>> =
        let rec innerMany (stream: Stream) =
            match p stream with
            | Failure _ -> ([], stream)
            | Success (v, stream) ->
                let (rest, stream) = innerMany stream
                (v :: rest, stream)

        innerMany >> Success

    // Almost same as `many`, but at least 1 character must be parsed successfully.
    let some (p: Parser<'a>): Parser<list<'a>> =
        fun (stream: Stream) ->
            let attempt = (many p) stream
            if attempt = Success ([], stream) then Failure "Unexpected Token" else attempt

    let tryParse (p: Parser<'a>): Parser<'a> =
        fun (stream: Stream) ->
            let pos = stream.Position()
            match p stream with
            | Success (v, s) -> Success (v, s)
            | Failure e ->
                stream.BackTo(pos)
                Failure e

    // Try to match a parser zero or one time.
    let opt (p: Parser<'a>): Parser<option<'a>> =
        let some = p |>> Some
        let none = returnP None
        some <|> none

    let sepBy1 (p: Parser<'a>) (sep: Parser<'b>): Parser<list<'a>> =
        let sepThenP = sep >>. p
        p
        .>>. some sepThenP
        |>> (fun (p, pList) -> p :: pList)

    let sepBy (p: Parser<'a>) (sep: Parser<'b>): Parser<list<'a>> = sepBy1 p sep <|> returnP []
