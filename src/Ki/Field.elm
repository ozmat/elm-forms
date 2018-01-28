module Ki.Field exposing (..)

import Dict as D exposing (Dict)
import Ki.Value as V exposing (Value)


-- Field


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



-- Maybe


maybeValue : Field comparable -> (Value -> a) -> Maybe a
maybeValue field f =
    case field of
        FieldValue value ->
            Just (f value)

        _ ->
            Nothing


maybeGroup : Field comparable -> (Group comparable -> a) -> Maybe a
maybeGroup field f =
    case field of
        FieldGroup g ->
            Just (f g)

        _ ->
            Nothing


liftMaybe : Maybe (Maybe a) -> Maybe a
liftMaybe maybe =
    case maybe of
        Just m ->
            m

        Nothing ->
            Nothing



-- Update


updateValue_ : Value -> Field comparable -> Field comparable
updateValue_ newValue field =
    mapValue (always newValue) field


updateValue : Value -> Maybe (Field comparable) -> Maybe (Field comparable)
updateValue newValue field =
    Maybe.map (updateValue_ newValue) field


updateGroup_ : comparable -> Value -> Field comparable -> Field comparable
updateGroup_ comparable newValue field =
    mapGroup (D.update comparable (updateValue newValue)) field


updateGroup : comparable -> Value -> Maybe (Field comparable) -> Maybe (Field comparable)
updateGroup comparable newValue field =
    Maybe.map (updateGroup_ comparable newValue) field



-- Walk through Group


walkGroup : comparable -> Group comparable -> Maybe (Field comparable)
walkGroup comparable group =
    let
        walk k v acc =
            case acc of
                Just _ ->
                    acc

                Nothing ->
                    -- liftMaybe (maybeGroup v (walkGroup comparable))
                    case v of
                        FieldGroup g ->
                            walkGroup comparable g

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
    case walkGroup comparable group of
        Nothing ->
            Nothing

        Just field ->
            maybeValue field identity
