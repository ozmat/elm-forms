module Forms.Field.Internal exposing
    ( Field(..)
    , Fields
    , getGroup
    , getValue
    , setValue
    )

import Dict as D exposing (Dict)
import Forms.Value.Internal as IV exposing (Value)



-- Definition


type Field comparable
    = FieldValue Value
    | FieldGroup (Fields comparable)


type alias Fields comparable =
    Dict comparable (Field comparable)



-- Map


mapValue : (Value -> Value) -> Field comparable -> Field comparable
mapValue f field =
    case field of
        FieldValue v ->
            FieldValue (f v)

        _ ->
            field


mapGroup : (Fields comparable -> Fields comparable) -> Field comparable -> Field comparable
mapGroup f field =
    case field of
        FieldGroup g ->
            FieldGroup (f g)

        _ ->
            field



-- Update


updateValue : Value -> Maybe (Field comparable) -> Maybe (Field comparable)
updateValue value field =
    Maybe.map (mapValue (IV.safeUpdate value)) field


updateGroup : Fields comparable -> Maybe (Field comparable) -> Maybe (Field comparable)
updateGroup gr field =
    Maybe.map (mapGroup (always gr)) field



-- Fields traversal -> get


getField : comparable -> Fields comparable -> Maybe (Field comparable)
getField comparable gr =
    let
        walk k v acc =
            case acc of
                Just _ ->
                    acc

                Nothing ->
                    case v of
                        FieldGroup g ->
                            getField comparable g

                        _ ->
                            Nothing
    in
    case D.get comparable gr of
        Just field ->
            Just field

        Nothing ->
            D.foldl walk Nothing gr


{-| Retrieves the `Value` associated with a key. If the key is not found or
the `Field` is not a `FieldValue` returns `Nothing`

    someFormFields : Fields comparable
    someFormFields =
        fields
            [ ( comparable1, string )
            , ( comparable2, bool )
            , ( comparable3, group [] )
            ]

    getValue comparable1 someFormFields -- Just (String ...)
    getValue comparable2 someFormFields -- Just (Bool ...)
    getValue comparable3 someFormFields -- Nothing
    getValue notfound someFormFields    -- Nothing

-}
getValue : comparable -> Fields comparable -> Maybe Value
getValue comparable gr =
    case getField comparable gr of
        Just field ->
            case field of
                FieldValue value ->
                    Just value

                _ ->
                    Nothing

        Nothing ->
            Nothing


{-| Retrieves the fieldgroup associated with a key. If the key is not found or
the `Field` is not a `FieldGroup` returns `Nothing`

    someFormFields : Fields comparable
    someFormFields =
        fields
            [ ( comparable1, string )
            , ( comparable2, bool )
            , ( comparable3, group [] )
            ]

    getGroup comparable1 someFormFields -- Nothing
    getGroup comparable2 someFormFields -- Nothing
    getGroup comparable3 someFormFields -- Just (Dict.fromList [ ... ])
    getGroup notfound someFormFields    -- Nothing

-}
getGroup : comparable -> Fields comparable -> Maybe (Fields comparable)
getGroup comparable gr =
    case getField comparable gr of
        Just field ->
            case field of
                FieldGroup g ->
                    Just g

                _ ->
                    Nothing

        Nothing ->
            Nothing



-- Fields traversal -> set


{-| Updates the `Value` associated with a key. It will only update the `Value`
if the key is found, the `Field` is a `FieldValue` and the `Value` have the
same type

    someFormFields : Fields comparable
    someFormFields =
        fields
            [ ( comparable1, string )
            , ( comparable2, group [] )
            ]

    setValue comparable1 (String ...) someFormFields -- updates
    setValue comparable1 (Bool ...) someFormFields   -- doesn't update
    setValue comparable2 (...) someFormFields        -- doesn't update
    setValue notfound (...) someFormFields           -- doesn't update

-}
setValue : comparable -> Value -> Fields comparable -> Fields comparable
setValue comparable value gr =
    let
        walk k v acc =
            if k == comparable then
                D.update k (updateValue value) acc

            else
                case v of
                    FieldGroup g ->
                        D.update k (updateGroup (setValue comparable value g)) acc

                    _ ->
                        acc
    in
    D.foldl walk gr gr
