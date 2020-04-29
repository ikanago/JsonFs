namespace JsonFS

module Parse =
    let anyChar (stream: string) =
        if stream.Length = 0 then
            None
        else
            Some(string stream.[0])
