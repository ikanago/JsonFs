namespace JsonFS

open JsonFS.JsonValue
open JsonFS.Parse
open JsonFS.CharParsers
open JsonFS.Combinator

module ParseJson =
    let parseInteger =
        let minus = (opt (specificChar '-') |>> Option.toList)
        let digits = some digit
        parser {
            let! integer = minus <+> digits
            return integer |> System.String.Concat |> decimal |> JNumber
        }

    let parseArray (s: Stream) =
        let delimeter = (specificChar ',') .>>. ws
        let parseInner = sepBy parseInteger delimeter
        s |> (between (specificChar '[') (specificChar ']') parseInner |>> JArray)

    let parseSymbol (symbol: string) =
        let rec parseSymbolInner (symbol: list<char>) =
            match symbol with
            | [] -> returnP ()
            | c::s -> specificChar c >>. parseSymbolInner s
        parseSymbolInner (Seq.toList symbol)

    let parseNull =
        parser {
            do! parseSymbol "null"
            return JNull
        }

    let parseBoolean =
        parser {
            return! parseSymbol "true" >>. returnP (JBoolean true)
            return! parseSymbol "false" >>. returnP (JBoolean false)
        }

    let parseString =
        parser {
            do! specificChar '\"' >>. ignoreP
            let! str = many (satisfy ((<>) '\"')) |>> (System.String.Concat >> JString)
            do! specificChar '\"' >>. ignoreP
            return str
        }

    let parseValue = parseInteger
