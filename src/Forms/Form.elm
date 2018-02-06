module Forms.Form
    exposing
        ( Form(..)
        , form
        , validate
        , validateD
        )

{-| `Form` is the top level type of the library. It is built with a [`Group`](http://package.elm-lang.org/packages/ozmat/elm-forms/latest/Forms-Field#Group)
of `Field`s and a [`Validate`](http://package.elm-lang.org/packages/ozmat/elm-forms/latest/Forms-Validation#Validate) function.
Please refer to the [examples](https://github.com/ozmat/elm-forms/tree/master/examples) for a better understanding


# Definition

@docs Form


# Common Helpers

@docs form, validate, validateD

-}

import Dict as D exposing (Dict)
import Forms.Field as F exposing (Group)
import Forms.Validation as V exposing (Validate, FieldError)


{-| A `Form` is a `Group` of `Field`s and a `Validate` function
-}
type Form comparable err a
    = Form (Group comparable) (Validate comparable err a)



-- Common Helpers


{-| Helps creating a `Form`
-}
form : Group comparable -> Validate comparable err a -> Form comparable err a
form =
    Form


{-| Helps validating a `Form`. This will use the internal `Validate` function
and `Field`s to run the validation process, then it will convert the
`FormValidation` into a [`Result`](http://package.elm-lang.org/packages/elm-lang/core/latest/Result#Result).
-}
validate : Form comparable err a -> Result (List ( comparable, FieldError err )) a
validate (Form fields validate) =
    V.toResult (validate fields)


{-| Equivalent to the previous one but returns a `Dict` of errors, in case of
failure, instead of a `List`.
-}
validateD : Form comparable err a -> Result (Dict comparable (FieldError err)) a
validateD fo =
    Result.mapError D.fromList (validate fo)
