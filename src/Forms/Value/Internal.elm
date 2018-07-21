module Forms.Value.Internal
    exposing
        ( Value(..)
        , safeUpdate
        )

-- Definition


type Value
    = String String
    | Bool Bool



-- Helpers


{-| Only updates `Value` of same type

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
