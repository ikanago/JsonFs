namespace JsonFS

open JsonFS.JsonValue
open JsonFS.Parse
open JsonFS.CharParsers
open JsonFS.Combinator

module ParseJson =
    let parseInteger =
        parser {
            let! minus = opt (specificChar '-') |>> Option.toList
            let! digits = some digit
            return minus @ digits |> System.String.Concat |> decimal |> JNumber
        }

    let parseArray (s: Stream) =
        let delimeter = (specificChar ',') .>>. ws
        let parseInner = sepBy parseInteger delimeter
        s |> (between (specificChar '[') (specificChar ']') parseInner |>> JArray)

    let parseValue = parseInteger
