module Test.Parse

open NUnit.Framework
open JsonFS.Parse

exception UnexpectedConditionException of string

[<Test>]
let StreamTest () =
    let stream = Stream "ab"
    match stream.Peek() with
    | Ok c ->
        Assert.AreEqual('a', c)
        stream.Skip() |> ignore
    | Error _ -> ()

    match stream.Peek() with
    | Ok c ->
        Assert.AreEqual('b', c)
        stream.Skip() |> ignore
    | Error _ -> ()

    match stream.Peek() with
    | Error "EOF" -> ()
    | _ -> raise (UnexpectedConditionException("Unreachable"))
