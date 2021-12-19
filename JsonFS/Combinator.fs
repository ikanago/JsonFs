namespace JsonFS

open JsonFS.Parse

module Combinator =
    // Apply both parser `p` and `q`.
    // If both of them succeed, return results wrapped into tuple.
    // If one of them fails, report the failure.
    let (.>>.) (p: Parser<'a>) (q: Parser<'b>) =
        fun (stream: Stream) ->
            result {
                let! pResult = p stream
                let! qResult = q stream
                return (pResult, qResult)
            }

    // First, apply the parser `p`.
    // If `p` succeeds, return the result of `p`.
    // If `p` fails, apply the parser `q` and return the result of `q`.
    let (<|>) (p: Parser<'a>) (q: Parser<'a>) =
        fun (stream: Stream) ->
            let pResult = p stream
            match pResult with
            | Success _ -> pResult
            | Failure _ -> q stream

    // First, apply the parser `p`.
    // If `p` succeeds, then apply f to the result of `p` and
    // return it as `ParseResult<'a>`.
    let (|>>) (p: Parser<'a>) f =
        fun (stream: Stream) ->
            result {
                let! r = p stream
                return f r
            }

    // Apply parsers `p` and `q` in sequence and throw away the result of `q`.
    let (.>>) (p: Parser<'a>) (q: Parser<'b>) = (p .>>. q) |>> (fun (a, _) -> a)

    // Apply parsers `p` and `q` in sequence and throw away the result of `p`.
    let (>>.) (p: Parser<'a>) (q: Parser<'b>) = (p .>>. q) |>> (fun (_, b) -> b)

    // Used to parse something surrounded by specific string parsed by `popen` and `pclose`.
    // For example, "{abc}".
    let between (popen: Parser<'a>) (pclose: Parser<'b>) (p: Parser<'c>) = popen >>. p .>> pclose

    let (<*>) (fP: Parser<'a -> 'b>) (xP: Parser<'a>): Parser<'b> = (fP .>>. xP) |>> (fun (f, x) -> f x)

    let lift2 f xP yP = (returnP f <*> xP <*> yP)

    // Apply parsers in `pList`.
    let rec sequence (pList: Parser<'a> list) =
        let cons (head: 'a) (tail: 'a list) = head :: tail
        let consP = lift2 cons
        match pList with
        | [] -> returnP []
        | headP :: tailP -> consP headP (sequence tailP)

    // Parse greedy as long as possible.
    let many (p: Parser<'a>) =
        let rec innerMany (stream: Stream) =
            let firstResult = p stream
            match firstResult with
            | Failure _ -> []
            | Success value -> value :: (innerMany stream)

        innerMany >> Success

    // Almost same as `many`, but at least 1 character must be parsed successfully.
    let many1 (p: Parser<'a>) =
        fun (stream: Stream) ->
            let attempt = (many p) stream
            if attempt = Success [] then Failure "Unexpected Token" else attempt

    // Try to match a parser zero or one time.
    let opt (p: Parser<'a>) =
        let some = p |>> Some
        let none = returnP None
        some <|> none

    let sepBy1 (p: Parser<'a>) (sep: Parser<'b>) =
        let sepThenP = sep >>. p
        p
        .>>. many1 sepThenP
        |>> (fun (p, pList) -> p :: pList)

    let sepBy (p: Parser<'a>) (sep: Parser<'b>) = sepBy1 p sep <|> returnP []
