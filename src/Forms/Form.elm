module Forms.Form exposing
    ( Form, form, validate
    , getStringField, getBoolField, setStringField, setBoolField
    )

{-| `Form` is the top level type of the library. It is built with [`Fields`](http://package.elm-lang.org/packages/ozmat/elm-forms/latest/Forms-Field#Fields)
and a [`Validate`](http://package.elm-lang.org/packages/ozmat/elm-forms/latest/Forms-Validation#Validate) function


# Forms

@docs Form, form, validate


# Field getters and setters

Sometimes you might need to get or set the current value of a `Field`. That is
what those functions are for

@docs getStringField, getBoolField, setStringField, setBoolField

-}

import Forms.Field.Internal as IF exposing (Fields)
import Forms.Form.Internal as Internal exposing (Form(..))
import Forms.Validation.Internal as IV exposing (Validate)
import Forms.Validation.Result exposing (FormResult)
import Forms.Value as V


{-| A `Form` is made up of `Fields` and a `Validate` function
-}
type alias Form comparable err a =
    Internal.Form comparable err a


{-| Creates a `Form`

    form someFormFields someFormValidate

-}
form : Fields comparable -> Validate comparable err a -> Form comparable err a
form =
    Form


{-| Validates a `Form`. This runs the validation process and returns
a `FormResult`
-}
validate : Form comparable err a -> FormResult comparable err a
validate (Form fields vf) =
    IV.toFormResult (vf fields)



-- Field getters and setters


{-| Gets the value of a string `Field` (input/select)
-}
getStringField : comparable -> Form comparable err a -> Maybe String
getStringField key (Form fields _) =
    IF.getValue key fields
        |> Maybe.andThen V.getString


{-| Gets the value of a bool `Field` (checkbox)
-}
getBoolField : comparable -> Form comparable err a -> Maybe Bool
getBoolField key (Form fields _) =
    IF.getValue key fields
        |> Maybe.andThen V.getBool


{-| Sets the value of a string `Field`
-}
setStringField : comparable -> String -> Form comparable err a -> Form comparable err a
setStringField key val (Form fields vf) =
    Form (IF.setValue key (V.string val) fields) vf


{-| Sets the value of a bool `Field`
-}
setBoolField : comparable -> Bool -> Form comparable err a -> Form comparable err a
setBoolField key val (Form fields vf) =
    Form (IF.setValue key (V.bool val) fields) vf
