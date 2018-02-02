module Forms.Value
    exposing
        ( Value(..)
        , string
        , bool
        , defaultString
        , defaultBool
        , safeUpdate
        )

-- Value
-- text input and checkbox


type Value
    = String String
    | Bool Bool



-- Wrapper


string : String -> Value
string =
    String


bool : Bool -> Value
bool =
    Bool



-- Default


defaultString : Value
defaultString =
    String ""


defaultBool : Value
defaultBool =
    Bool False



-- Safe update


safeUpdate : Value -> Value -> Value
safeUpdate value original =
    case ( value, original ) of
        ( String s, String _ ) ->
            String s

        ( Bool b, Bool _ ) ->
            Bool b

        _ ->
            original
