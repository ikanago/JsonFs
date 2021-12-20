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

    let parseNull =
        parser {
            do! specificChar 'n' >>. ignoreP
            do! specificChar 'u' >>. ignoreP
            do! specificChar 'l' >>. ignoreP
            do! specificChar 'l' >>. ignoreP
            return JNull
        }

    let parseString =
        parser {
            do! specificChar '\"' >>. ignoreP
            let! str = many (satisfy ((<>) '\"')) |>> (System.String.Concat >> JString)
            do! specificChar '\"' >>. ignoreP
            return str
        }

    let parseValue = parseInteger
