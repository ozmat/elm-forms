module Forms.Form
    exposing
        ( Form(..)
        , form
        , validate
        , validateWithFieldErrors
        )

{-| `Form` is the top level type of the library. It is built with [`Fields`](http://package.elm-lang.org/packages/ozmat/elm-forms/latest/Forms-Field#Fields)
and a [`Validate`](http://package.elm-lang.org/packages/ozmat/elm-forms/latest/Forms-Validation#Validate) function.
Please refer to the [examples](https://github.com/ozmat/elm-forms/tree/master/examples) for a better understanding


# Definition

@docs Form


# Common Helpers

@docs form, validate, validateWithFieldErrors

-}

import Dict as D exposing (Dict)
import Forms.Field as F exposing (Fields)
import Forms.Validation as V exposing (FormResult, Validate, FieldError)


{-| A `Form` is a group of `Field`s and a `Validate` function
-}
type Form comparable err a
    = Form (Fields comparable) (Validate comparable err a)



-- Common Helpers


{-| Creates a `Form`
-}
form : Fields comparable -> Validate comparable err a -> Form comparable err a
form =
    Form


{-| Validates a `Form` using its `Validate` function
and `Fields`. It will run the validation process and converts the
`FormValidation` into a `FormResult`.
-}
validate : Form comparable err a -> FormResult comparable err a
validate (Form fields validate) =
    V.toFormResult (validate fields)


{-| Old API - Returns the `FieldError`s without using `FormResult`
-}
validateWithFieldErrors : Form comparable err a -> Result (Dict comparable (FieldError err)) a
validateWithFieldErrors (Form fields validate) =
    Result.mapError D.fromList (V.toResult (validate fields))
