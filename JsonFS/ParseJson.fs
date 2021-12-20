namespace JsonFS

open JsonFS.JsonValue
open JsonFS.Parse
open JsonFS.CharParsers
open JsonFS.Combinator

module ParseJson =
    let parseInteger =
        parser {
            let! minus = opt (specificChar '-') |>> Option.toList
            let! digits = many1 digit
            return minus @ digits |> System.String.Concat |> decimal |> JNumber
        }
