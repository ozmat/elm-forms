module Ki.Field exposing (..)

import Dict as D exposing (Dict)
import Ki.Value as V exposing (Value)


-- Field
-- TODO Implement our RB Tree instead of using the Dict one ?


type Field comparable
    = FieldGroup (Group comparable)
    | FieldValue Value


type alias Group comparable =
    Dict comparable (Field comparable)


string : Field comparable
string =
    FieldValue V.defaultString


bool : Field comparable
bool =
    FieldValue V.defaultBool


group : List ( comparable, Field comparable ) -> Field comparable
group g =
    FieldGroup (D.fromList g)


fields : List ( comparable, Field comparable ) -> Group comparable
fields =
    D.fromList



-- With custom value


stringWithValue : String -> Field comparable
stringWithValue s =
    FieldValue (V.string s)


boolWithValue : Bool -> Field comparable
boolWithValue b =
    FieldValue (V.bool b)



-- Map


mapValue : (Value -> Value) -> Field comparable -> Field comparable
mapValue f field =
    case field of
        FieldValue v ->
            FieldValue (f v)

        _ ->
            field


mapGroup : (Group comparable -> Group comparable) -> Field comparable -> Field comparable
mapGroup f field =
    case field of
        FieldGroup g ->
            FieldGroup (f g)

        _ ->
            field



-- Update


updateValue : Value -> Maybe (Field comparable) -> Maybe (Field comparable)
updateValue value field =
    Maybe.map (mapValue (V.safeUpdate value)) field


updateGroup : Group comparable -> Maybe (Field comparable) -> Maybe (Field comparable)
updateGroup group field =
    Maybe.map (mapGroup (always group)) field



-- Walk through Group -> get


getField : comparable -> Group comparable -> Maybe (Field comparable)
getField comparable group =
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
        case D.get comparable group of
            Just field ->
                Just field

            Nothing ->
                D.foldl walk Nothing group


getValue : comparable -> Group comparable -> Maybe Value
getValue comparable group =
    case getField comparable group of
        Just field ->
            case field of
                FieldValue value ->
                    Just value

                _ ->
                    Nothing

        Nothing ->
            Nothing


getGroup : comparable -> Group comparable -> Maybe (Group comparable)
getGroup comparable group =
    case getField comparable group of
        Just field ->
            case field of
                FieldGroup g ->
                    Just g

                _ ->
                    Nothing

        Nothing ->
            Nothing



-- Walk through Group -> set


setValue : comparable -> Value -> Group comparable -> Group comparable
setValue comparable value group =
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
        D.foldl walk group group
