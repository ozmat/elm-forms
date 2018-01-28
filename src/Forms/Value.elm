module Forms.Value
    exposing
        ( Value(..)
        , stringValue
        , booleanValue
        , isEmpty
          -- Validates
        , strValidate
        , strValidates
        , strGT
        , strGTE
        , strLT
        , strLTE
        , booleanChecked
        )

import Forms.Validation exposing (Validate)
import List.Nonempty as NE exposing (Nonempty)


-- W3C - /^[a-zA-Z0-9.!#$%&’*+/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:\.[a-zA-Z0-9-]+)*$/
-- http://emailregex.com/ - /^(([^<>()\[\]\\.,;:\s@"]+(\.[^<>()\[\]\\.,;:\s@"]+)*)|(".+"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$/
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



-- Helpers


isEmpty : Value -> Bool
isEmpty v =
    case v of
        Str x ->
            String.isEmpty x

        _ ->
            False



-- Enforce Validate


strValidate : Validate String -> Validate Value
strValidate sv =
    \v ->
        case v of
            Str s ->
                sv s

            _ ->
                False


strValidates : Nonempty ( err, Validate String ) -> Nonempty ( err, Validate Value )
strValidates svs =
    NE.map (Tuple.mapSecond strValidate) svs


booleanValidate : Validate Bool -> Validate Value
booleanValidate bv =
    \v ->
        case v of
            Boolean b ->
                bv b

            _ ->
                False



-- Basic validate


strGT : Int -> Validate Value
strGT n =
    strValidate (\s -> String.length s > n)


strGTE : Int -> Validate Value
strGTE n =
    strValidate (\s -> String.length s >= n)


strLT : Int -> Validate Value
strLT n =
    strValidate (\s -> String.length s < n)


strLTE : Int -> Validate Value
strLTE n =
    strValidate (\s -> String.length s <= n)


booleanChecked : Validate Value
booleanChecked =
    booleanValidate identity
