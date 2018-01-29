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


string : String -> Field comparable
string s =
    FieldValue (V.string s)


bool : Bool -> Field comparable
bool b =
    FieldValue (V.bool b)


group : List ( comparable, Field comparable ) -> Field comparable
group g =
    FieldGroup (D.fromList g)



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
    Maybe.map (mapValue (always value)) field


updateGroup : Group comparable -> Maybe (Field comparable) -> Maybe (Field comparable)
updateGroup group field =
    Maybe.map (mapGroup (always group)) field



-- Walk through Group -> get


getValue : comparable -> Group comparable -> Maybe Value
getValue comparable group =
    let
        walk k v acc =
            case acc of
                Just _ ->
                    acc

                Nothing ->
                    case v of
                        FieldGroup g ->
                            getValue comparable g

                        _ ->
                            Nothing
    in
        case D.get comparable group of
            Just field ->
                case field of
                    FieldValue value ->
                        Just value

                    _ ->
                        Nothing

            Nothing ->
                D.foldl walk Nothing group



-- Walk through Group -> set
-- TODO test setValue : it could replace a Str with a Bool ? use a safe updateValue ?


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
