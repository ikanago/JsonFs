namespace JsonFS

type JsonValue =
    | JObject of members: (string * JsonValue) []
    | JArray of elements: JsonValue []
    | JString of string
    | JNumber of decimal
    | JFloat of float
    | JBoolean of bool
    | JNull

module JsonValue =
    let object = JString "hoge"
