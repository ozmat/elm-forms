module Forms.Value
    exposing
        ( Value
        , bool
        , defaultBool
        , defaultString
        , getBool
        , getString
        , string
        )

{-| A `Value` represents the value inside a [`Field`](http://package.elm-lang.org/packages/ozmat/elm-forms/latest/Forms-Field#Field)


# Definition

@docs Value


# Creation

@docs string, bool


# Default value

@docs defaultString, defaultBool


# Get value

@docs getString, getBool

-}

import Forms.Value.Internal as Internal exposing (Value(..))


{-| A `Value` can be a `String` (input, select) or a `Bool` (checkbox)
-}
type alias Value =
    Internal.Value



-- Creation


{-| Creates a `String` `Value`

    string "some input value"

-}
string : String -> Value
string =
    String


{-| Creates a `Bool` `Value`

    bool True

-}
bool : Bool -> Value
bool =
    Bool



-- Default


{-| Is the default `String` `Value` (empty string)
-}
defaultString : Value
defaultString =
    String ""


{-| Is the default `Bool` `Value` (false)
-}
defaultBool : Value
defaultBool =
    Bool False



-- Test


{-| Returns the value of a `String` `Value`
-}
getString : Value -> Maybe String
getString v =
    case v of
        String s ->
            Just s

        _ ->
            Nothing


{-| Returns the value of a `Bool` `Value`
-}
getBool : Value -> Maybe Bool
getBool v =
    case v of
        Bool b ->
            Just b

        _ ->
            Nothing
