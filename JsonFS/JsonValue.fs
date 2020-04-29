namespace JsonFS

type JsonValue =
    | Object of members: (string * JsonValue)[]
    | Array of elements: JsonValue[]
    | String of string
    | Number of decimal
    | Float of float
    | Boolean of bool
    | Null

module JsonValue =
    let object = String "hoge"
