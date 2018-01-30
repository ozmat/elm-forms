module Ki.Form exposing (..)

import Ki.Field as F exposing (Group)
import Ki.Validation as V exposing (Validate, FormError, FormValidation)


-- TODO Refactor using union type ?


type alias Form comparable err a =
    { fields : Group comparable
    , validate : Validate comparable err a
    }


form : Group comparable -> Validate comparable err a -> Form comparable err a
form =
    Form


validate : Form comparable err a -> Result (List (FormError comparable err)) a
validate form =
    case form.validate form.fields of
        V.Success a ->
            Ok a

        V.Failure ve ->
            Err (V.toList ve)


setFields : Group comparable -> Form comparable err a -> Form comparable err a
setFields group form =
    { form | fields = group }
