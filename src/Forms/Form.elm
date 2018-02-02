module Forms.Form
    exposing
        ( Form(..)
        , form
        , validate
        , validateD
        )

import Dict as D exposing (Dict)
import Vi.Validation as VA exposing (Validation(..))
import Forms.Field as F exposing (Group)
import Forms.Validation as V exposing (Validate, FieldError, FormError)


-- Form


type Form comparable err a
    = Form (Group comparable) (Validate comparable err a)


form : Group comparable -> Validate comparable err a -> Form comparable err a
form =
    Form



-- Validation


validate : Form comparable err a -> Result (List ( comparable, FieldError err )) a
validate (Form fields validate) =
    case validate fields of
        Success a ->
            Ok a

        Failure ve ->
            Err (toTupleList ve)


validateD : Form comparable err a -> Result (Dict comparable (FieldError err)) a
validateD (Form fields validate) =
    case validate fields of
        Success a ->
            Ok a

        Failure ve ->
            Err (D.fromList (toTupleList ve))


toTupleList : VA.ValidationError (FormError comparable err) -> List ( comparable, FieldError err )
toTupleList ve =
    List.map V.toTuple (VA.toList ve)
