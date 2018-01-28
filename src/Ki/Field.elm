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
