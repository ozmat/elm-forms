module Forms.Value
    exposing
        ( Value(..)
        , stringValue
        , booleanValue
        , isEmpty
        )

-- Value
-- input and checkbox
-- TODO select


type Value
    = Str String
    | Boolean Bool


stringValue : Value
stringValue =
    Str ""


booleanValue : Value
booleanValue =
    Boolean False


isEmpty : Value -> Bool
isEmpty v =
    case v of
        Str x ->
            String.isEmpty x

        Boolean _ ->
            False
