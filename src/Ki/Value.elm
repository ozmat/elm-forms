module Ki.Value exposing (..)

-- Value
-- text input and checkbox


type Value
    = String String
    | Bool Bool


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


defaultBoolean : Value
defaultBoolean =
    Bool False



-- Helpers


isEmpty : Value -> Maybe Bool
isEmpty value =
    case value of
        String x ->
            Just (String.isEmpty x)

        _ ->
            Nothing
