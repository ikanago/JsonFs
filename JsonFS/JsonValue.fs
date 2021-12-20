namespace JsonFS

module JsonValue =
    type JsonValue =
        | JObject of members: list<(string * JsonValue)>
        | JArray of elements: list<JsonValue>
        | JString of string
        | JNumber of decimal
        | JFloat of float
        | JBoolean of bool
        | JNull
