namespace JsonFS

open JsonFS.JsonValue
open JsonFS.Parse
open JsonFS.CharParsers
open JsonFS.Combinator

module ParseJson =
    let parseInteger = fun (stream: Stream) ->
        match many digit stream with
        | Failure _ -> Failure "Unexpected Token"
        | Success digits -> digits |> System.String.Concat |> decimal |> JNumber |> Success
