module Forms.Value
    exposing
        ( Value(..)
        , stringValue
        , booleanValue
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
