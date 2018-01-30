module Ki.Form exposing (..)

import Ki.Field as F exposing (Group)
import Ki.Validation as V exposing (Validate, FormError, FormValidation)


type Form comparable err a
    = Form (Group comparable) (Validate comparable err a)


form : Group comparable -> Validate comparable err a -> Form comparable err a
form =
    Form


validate : Form comparable err a -> Result (List (FormError comparable err)) a
validate (Form fields validate) =
    case validate fields of
        V.Success a ->
            Ok a

        V.Failure ve ->
            Err (V.toList ve)
