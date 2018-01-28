module Forms.Value
    exposing
        ( Value(..)
        , stringValue
        , booleanValue
        , isEmpty
        )

import Forms.Validation exposing (Validate, bindValidate, bindValidates)
import List.Nonempty as NE exposing (Nonempty)


-- Value
-- text input and checkbox
-- TODO select


type Value
    = Str String
    | Boolean Bool



-- Default Value


stringValue : Value
stringValue =
    Str ""


booleanValue : Value
booleanValue =
    Boolean False



-- Enforce Validate


strValidate : Validate String -> Validate Value
strValidate sv =
    \v ->
        case v of
            Str s ->
                sv s

            _ ->
                False


strValidates : Nonempty (Validate String) -> Validate Value
strValidates vss =
    bindValidates (NE.map strValidate vss)


booleanValidate : Validate Bool -> Validate Value
booleanValidate bv =
    \v ->
        case v of
            Boolean b ->
                bv b

            _ ->
                False



-- Helpers


isEmpty : Value -> Bool
isEmpty v =
    case v of
        Str x ->
            String.isEmpty x

        Boolean _ ->
            False
