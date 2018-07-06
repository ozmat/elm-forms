module Forms.Form
    exposing
        ( Form(..)
        , form
        , validate
        , validateD
        )

{-| `Form` is the top level type of the library. It is built with [`Fields`](http://package.elm-lang.org/packages/ozmat/elm-forms/latest/Forms-Field#Fields)
and a [`Validate`](http://package.elm-lang.org/packages/ozmat/elm-forms/latest/Forms-Validation#Validate) function.
Please refer to the [examples](https://github.com/ozmat/elm-forms/tree/master/examples) for a better understanding


# Definition

@docs Form


# Common Helpers

@docs form, validate, validateD

-}

import Dict as D exposing (Dict)
import Forms.Field as F exposing (Fields)
import Forms.Validation as V exposing (Validate, FieldError)


{-| A `Form` is a group of `Field`s and a `Validate` function
-}
type Form comparable err a
    = Form (Fields comparable) (Validate comparable err a)



-- Common Helpers


{-| Helps creating a `Form`
-}
form : Fields comparable -> Validate comparable err a -> Form comparable err a
form =
    Form


{-| Helps validating a `Form`. This will use the internal `Validate` function
and `Fields` to run the validation process, then it will convert the
`FormValidation` into a [`Result`](http://package.elm-lang.org/packages/elm-lang/core/latest/Result#Result)
-}
validate : Form comparable err a -> Result (List ( comparable, FieldError err )) a
validate (Form fields validate) =
    V.toResult (validate fields)


{-| Equivalent to the previous one but in case of failure it returns
a `Dict` of errors instead of a `List`
-}
validateD : Form comparable err a -> Result (Dict comparable (FieldError err)) a
validateD fo =
    Result.mapError D.fromList (validate fo)
