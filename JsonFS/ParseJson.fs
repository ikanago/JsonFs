namespace JsonFS

open JsonFS.JsonValue
open JsonFS.Parse
open JsonFS.CharParsers
open JsonFS.Combinator

module ParseJson =
    let parseInteger = fun (stream: Stream) ->
        result {
            let! minus = stream |> (opt (specificChar '-') |>> Option.toList)
            let! digits = many1 digit stream
            return minus @ digits |> System.String.Concat |> decimal |> JNumber
        }
