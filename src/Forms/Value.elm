module Forms.Value
    exposing
        ( Value(..)
        , string
        , bool
        , defaultString
        , defaultBool
        , safeUpdate
        )

{-| A `Value` represents the value inside a [`Field`](http://package.elm-lang.org/packages/ozmat/elm-forms/latest/Forms-Field#Field).
Please refer to the [basic examples]() (or [advanced examples]()) for a better
understanding


# Definition

@docs Value


# Common Helpers

@docs string, bool, defaultString, defaultBool


# Update

@docs safeUpdate

-}


{-| A `Value` can be a `String` (form input) or a `Bool` (form checkbox)

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
