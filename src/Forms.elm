module Forms
    exposing
        ( FieldValidation
        , Fields
        , Form
        , FormResult
        , Msg
        , Validate
        , boolField
        , boolFieldMsg
        , checkbox
        , discardable
        , failure
        , fieldgroup
        , fields
        , form
        , group
        , hardcoded
        , input
        , optional
        , optionalWithMaybe
        , required
        , select
        , stringField
        , stringFieldMsg
        , success
        , twoFields
        , updateForm
        , valid
        , validate
        )

{-| This module only re-exports all the common types and functions of the
library. You can either import them from here or their own modules


# Module Forms.Field

@docs Fields, fields, input, select, checkbox, group


# Module Forms.Form

@docs Form, form, validate


# Module Forms.Update

@docs Msg, stringFieldMsg, boolFieldMsg, updateForm


# Module Forms.Validation

@docs FieldValidation, success, failure, stringField, boolField, valid, Validate, required, hardcoded, optional, optionalWithMaybe, discardable, twoFields, fieldgroup


# Module Forms.Validation.Result

@docs FormResult

-}

-- Issue : https://github.com/elm/compiler/issues/931

import Forms.Field as FI
import Forms.Form as FO
import Forms.Update as FU
import Forms.Validation as FV
import Forms.Validation.Result as FVR
import Forms.Value as FVA


-- Module Forms.Field


{-| -}
type alias Fields comparable =
    FI.Fields comparable


{-| -}
input : FI.Field comparable
input =
    FI.input


{-| -}
select : FI.Field comparable
select =
    FI.select


{-| -}
checkbox : FI.Field comparable
checkbox =
    FI.checkbox


{-| -}
group : List ( comparable, FI.Field comparable ) -> FI.Field comparable
group =
    FI.group


{-| -}
fields : List ( comparable, FI.Field comparable ) -> Fields comparable
fields =
    FI.fields



-- Module Forms.Form


{-| -}
type alias Form comparable err a =
    FO.Form comparable err a


{-| -}
form : Fields comparable -> FV.Validate comparable err a -> Form comparable err a
form =
    FO.form


{-| -}
validate : Form comparable err a -> FVR.FormResult comparable err a
validate =
    FO.validate



-- Module Forms.Update


{-| -}
type alias Msg comparable =
    FU.Msg comparable


{-| -}
stringFieldMsg : (Msg comparable -> msg) -> comparable -> (String -> msg)
stringFieldMsg =
    FU.stringFieldMsg


{-| -}
boolFieldMsg : (Msg comparable -> msg) -> comparable -> (Bool -> msg)
boolFieldMsg =
    FU.boolFieldMsg


{-| -}
updateForm : Msg comparable -> Form comparable err a -> Form comparable err a
updateForm =
    FU.updateForm



-- Module Forms.Validation


{-| -}
type alias FieldValidation err a =
    FV.FieldValidation err a


{-| -}
success : a -> FieldValidation err a
success =
    FV.success


{-| -}
failure : err -> FieldValidation err a
failure =
    FV.failure


{-| -}
stringField : (String -> FieldValidation err a) -> FVA.Value -> FieldValidation err a
stringField =
    FV.stringField


{-| -}
boolField : (Bool -> FieldValidation err a) -> FVA.Value -> FieldValidation err a
boolField =
    FV.boolField


{-| -}
valid : a -> FV.FormValidation comparable err a
valid =
    FV.valid


{-| -}
type alias Validate comparable err a =
    FV.Validate comparable err a


{-| -}
required : Fields comparable -> comparable -> (FVA.Value -> FieldValidation err a) -> FV.FormValidation comparable err (a -> b) -> FV.FormValidation comparable err b
required =
    FV.required


{-| -}
hardcoded : a -> FV.FormValidation comparable err (a -> b) -> FV.FormValidation comparable err b
hardcoded =
    FV.hardcoded


{-| -}
optional : Fields comparable -> comparable -> a -> (String -> FieldValidation err a) -> FV.FormValidation comparable err (a -> b) -> FV.FormValidation comparable err b
optional =
    FV.optional


{-| -}
optionalWithMaybe : Fields comparable -> comparable -> (String -> FieldValidation err a) -> FV.FormValidation comparable err (Maybe a -> b) -> FV.FormValidation comparable err b
optionalWithMaybe =
    FV.optionalWithMaybe


{-| -}
discardable : Fields comparable -> comparable -> (FVA.Value -> FieldValidation err a) -> FV.FormValidation comparable err b -> FV.FormValidation comparable err b
discardable =
    FV.discardable


{-| -}
twoFields : Fields comparable -> comparable -> comparable -> (FVA.Value -> FVA.Value -> FieldValidation err a) -> FV.FormValidation comparable err (a -> b) -> FV.FormValidation comparable err b
twoFields =
    FV.twoFields


{-| -}
fieldgroup : Fields comparable -> comparable -> (Fields comparable -> FV.FormValidation comparable err a) -> FV.FormValidation comparable err (a -> b) -> FV.FormValidation comparable err b
fieldgroup =
    FV.fieldgroup



-- Module Forms.Validation.Result


{-| -}
type alias FormResult comparable err a =
    FVR.FormResult comparable err a
