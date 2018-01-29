module Ki.Form exposing (..)

import Ki.Field as F exposing (Group)
import Ki.Validation as V exposing (Validate, FormValidation)


type alias Form comparable err a =
    { fields : Group comparable
    , validate : Validate comparable err a
    }


form : Group comparable -> Validate comparable err a -> Form comparable err a
form =
    Form


validate : Form comparable err a -> FormValidation err a
validate form =
    form.validate form.fields


setFields : comparable -> Group comparable -> Form comparable err a -> Form comparable err a
setFields comparable group form =
    { form | fields = group }
