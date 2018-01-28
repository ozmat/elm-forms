module Forms.Field
    exposing
        ( Field
        , mkField
        , updateValue
        , FieldType(..)
        , FieldValidation(..)
        , fieldValidate
        )

import List.Nonempty as NE exposing (Nonempty)
import Forms.Validation exposing (Validation(..), Validate, validate, accValidation)
import Forms.Value exposing (Value, isEmpty)


-- Field


type alias Field err =
    { value : Value
    , fType : FieldType
    , fValidation : FieldValidation err
    }


mkField : comparable -> Value -> FieldType -> FieldValidation err -> ( comparable, Field err )
mkField comparable value fType fValidation =
    ( comparable, Field value fType fValidation )


updateValue : Value -> Maybe (Field err) -> Maybe (Field err)
updateValue newValue field =
    case field of
        Nothing ->
            Nothing

        Just f ->
            Just { f | value = newValue }



-- Field type


type FieldType
    = Optional
    | Required



-- Field validation


type FieldValidation err
    = NoValidation
    | FieldValidation (Nonempty ( err, Validate Value ))


fieldValidate : Field err -> Validation (List err)
fieldValidate field =
    case field.fValidation of
        NoValidation ->
            ValidationSuccess

        FieldValidation validates ->
            case field.fType of
                Optional ->
                    if isEmpty field.value then
                        ValidationSuccess
                    else
                        fieldValidate_ field.value validates

                Required ->
                    fieldValidate_ field.value validates


fieldValidate_ : Value -> Nonempty ( err, Validate Value ) -> Validation (List err)
fieldValidate_ value validates =
    let
        validations =
            NE.map (uncurry (validate value)) validates
    in
        case accValidation [] (NE.toList validations) of
            [] ->
                ValidationSuccess

            ess ->
                ValidationFailure ess
