module Forms.Form
    exposing
        ( Form(..)
        , form
        , validate
        , validateD
        )

import Dict as D exposing (Dict)
import Validation as VA
import Forms.Field as F exposing (Group)
import Forms.Validation as V exposing (Validate, FieldError)


-- Form


type Form comparable err a
    = Form (Group comparable) (Validate comparable err a)


form : Group comparable -> Validate comparable err a -> Form comparable err a
form =
    Form



-- Validation


validate : Form comparable err a -> Result (List ( comparable, FieldError err )) a
validate (Form fields validate) =
    validate fields
        |> VA.toResult
        |> Result.mapError (List.map V.toTuple)


validateD : Form comparable err a -> Result (Dict comparable (FieldError err)) a
validateD fo =
    validate fo
        |> Result.mapError D.fromList
