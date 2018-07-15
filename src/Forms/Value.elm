module Forms.Value
    exposing
        ( Value(..)
        , bool
        , defaultBool
        , defaultString
        , isBool
        , isString
        , safeUpdate
        , string
        )

{-| A `Value` represents the value inside a [`Field`](http://package.elm-lang.org/packages/ozmat/elm-forms/latest/Forms-Field#Field).
Please refer to the [examples](https://github.com/ozmat/elm-forms/tree/master/examples) for a better understanding


# Definition

@docs Value


# Common Helpers

@docs string, bool, defaultString, defaultBool


# Test

@docs isString, isBool


# Update

@docs safeUpdate

-}


{-| A `Value` can be a `String` (input, select) or a `Bool` (checkbox)

    String "some input value"

    Bool True

-}
type Value
    = String String
    | Bool Bool



-- Common helpers


{-| Helps creating a `String` `Value`

    string "some input value" -- String "some input value"

-}
string : String -> Value
string =
    String


{-| Helps creating a `Bool` `Value`

    bool True -- Bool True

-}
bool : Bool -> Value
bool =
    Bool


{-| Is the default `String` `Value` (empty string)

    String ""

-}
defaultString : Value
defaultString =
    String ""


{-| Is the default `Bool` `Value` (false)

    Bool False

-}
defaultBool : Value
defaultBool =
    Bool False



-- Test


{-| Tests if the `Value` is a `String` and returns it
-}
isString : Value -> Maybe String
isString v =
    case v of
        String s ->
            Just s

        _ ->
            Nothing


{-| Tests if the `Value` is a `Bool` and returns it
-}
isBool : Value -> Maybe Bool
isBool v =
    case v of
        Bool b ->
            Just b

        _ ->
            Nothing



-- Update


{-| Only updates `Value`s of same type

    safeUpdate (String "new one") (String "old one") -- String "new one"
    safeUpdate (String "new one") (Bool False)       -- Bool False

-}
safeUpdate : Value -> Value -> Value
safeUpdate value original =
    case ( value, original ) of
        ( String s, String _ ) ->
            String s

        ( Bool b, Bool _ ) ->
            Bool b

        _ ->
            original
