module Ki.Form exposing (..)

import Dict as D exposing (Dict)
import Vi.Validation as VA exposing (Validation(..))
import Ki.Field as F exposing (Group)
import Ki.Validation as V exposing (Validate, FieldError, FormError)


type Form comparable err a
    = Form (Group comparable) (Validate comparable err a)


form : Group comparable -> Validate comparable err a -> Form comparable err a
form =
    Form


validate : Form comparable err a -> Result (List (FormError comparable err)) a
validate (Form fields validate) =
    case validate fields of
        Success a ->
            Ok a

        Failure ve ->
            Err (VA.toList ve)


validateD : Form comparable err a -> Result (Dict comparable (FieldError err)) a
validateD (Form fields validate) =
    case validate fields of
        Success a ->
            Ok a

        Failure ve ->
            Err (D.fromList (List.map V.toTuple (VA.toList ve)))
