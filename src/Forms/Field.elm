module Forms.Field
    exposing
        ( Field
        , mkField
        , updateValue
        , Value(..)
        , stringValue
        , booleanValue
        , FieldType(..)
        , FieldValidation(..)
        , fieldValidate
        )

import List.Nonempty as NE exposing (Nonempty)
import Forms.Validation exposing (Validation(..), Validate, validate, accValidation)


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



-- Value
-- input and checkbox
-- TODO select


type Value
    = Str String
    | Boolean Bool


stringValue : Value
stringValue =
    Str ""


booleanValue : Value
booleanValue =
    Boolean False



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
            let
                validations =
                    NE.map (uncurry (validate field.value)) validates
            in
                case accValidation [] (NE.toList validations) of
                    [] ->
                        ValidationSuccess

                    ess ->
                        ValidationFailure ess
