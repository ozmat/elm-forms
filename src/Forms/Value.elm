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

{-| `Value` represents the inner value of a [`Field`](http://package.elm-lang.org/packages/ozmat/elm-forms/latest/Forms-Field#Field)


# Values

@docs Value


# Default values

@docs defaultString, defaultBool


# Creation

@docs string, bool


# Access Values

@docs getString, getBool

-}

import Forms.Value.Internal as Internal exposing (Value(..))


{-| A `Value` can be a `String` (input/select field) or
a `Bool` (checkbox field)
-}
type alias Value =
    Internal.Value



-- Default values


{-| Is the default string `Value`

    ""

-}
defaultString : Value
defaultString =
    String ""


{-| Is the default bool `Value`

    False

-}
defaultBool : Value
defaultBool =
    Bool False



-- Creation


{-| Creates a string `Value`

    string "some input value"

-}
string : String -> Value
string =
    String


{-| Creates a bool `Value`

    bool True

-}
bool : Bool -> Value
bool =
    Bool



-- Access Values


{-| Returns the value of a string `Value`
-}
getString : Value -> Maybe String
getString v =
    case v of
        String s ->
            Just s

        _ ->
            Nothing


{-| Returns the value of a bool `Value`
-}
getBool : Value -> Maybe Bool
getBool v =
    case v of
        Bool b ->
            Just b

        _ ->
            Nothing
