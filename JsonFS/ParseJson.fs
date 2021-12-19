namespace JsonFS

open JsonFS.JsonValue
open JsonFS.Parse
open JsonFS.CharParsers
open JsonFS.Combinator

module ParseJson =
    let parseInteger = fun (stream: Stream) ->
        match stream |> (opt (specificChar '-') |>> Option.toList) with
        | Failure _ -> Failure "Unexpected Token"
        | Success minus ->
            match many1 digit stream with
                | Failure _ -> Failure "Unexpected Token"
                | Success digits -> minus @ digits |> System.String.Concat |> decimal |> JNumber |> Success
