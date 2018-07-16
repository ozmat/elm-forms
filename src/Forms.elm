module Forms
    exposing
        ( Form(..)
        , form
        , getBoolField
        , getStringField
        , setBoolField
        , setStringField
        , validate
        )

{-| `Form` is the top level type of the library. It is built with [`Fields`](http://package.elm-lang.org/packages/ozmat/elm-forms/latest/Forms-Field#Fields)
and a [`Validate`](http://package.elm-lang.org/packages/ozmat/elm-forms/latest/Forms-Validation#Validate) function.
Please refer to the [examples](https://github.com/ozmat/elm-forms/tree/master/examples) for a better understanding


# Definition

@docs Form


# Common Helpers

@docs form, validate


# Field getters and setters

@docs getStringField, getBoolField, setStringField, setBoolField

-}

import Forms.Field.Internal as IF exposing (Fields)
import Forms.Validation.Internal as IV exposing (Validate)
import Forms.Validation.Result exposing (FormResult)
import Forms.Value as V


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
    IV.toFormResult (validate fields)



-- Field getters and setters


{-| Gets the value of a `String` `Field`
-}
getStringField : comparable -> Form comparable err a -> Maybe String
getStringField key (Form fields _) =
    IF.getValue key fields
        |> Maybe.andThen V.getString


{-| Gets the value of a `Bool` `Field`
-}
getBoolField : comparable -> Form comparable err a -> Maybe Bool
getBoolField key (Form fields _) =
    IF.getValue key fields
        |> Maybe.andThen V.getBool


{-| Sets the value of a `String` `Field`
-}
setStringField : comparable -> String -> Form comparable err a -> Form comparable err a
setStringField key val (Form fields validate) =
    Form (IF.setValue key (V.string val) fields) validate


{-| Sets the value of a `String` `Field`
-}
setBoolField : comparable -> Bool -> Form comparable err a -> Form comparable err a
setBoolField key val (Form fields validate) =
    Form (IF.setValue key (V.bool val) fields) validate
