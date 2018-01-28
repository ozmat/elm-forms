module Forms.Validation
    exposing
        ( Validation(..)
        , validate
        , accValidation
        , accValidationDict
        , Validate
        , bindValidate
        , bindValidates
        )

import List.Nonempty as NE exposing (Nonempty)


-- Validation


type Validation err
    = ValidationFailure err
    | ValidationSuccess


validate : a -> err -> Validate a -> Validation err
validate a err test =
    if test a then
        ValidationSuccess
    else
        ValidationFailure err


accValidation : List err -> List (Validation err) -> List err
accValidation errors validations =
    case validations of
        [] ->
            errors

        h :: t ->
            case h of
                ValidationSuccess ->
                    accValidation errors t

                ValidationFailure err ->
                    accValidation (err :: errors) t


accValidationDict : List ( comparable, List err ) -> List ( comparable, Validation (List err) ) -> List ( comparable, List err )
accValidationDict errors validations =
    case validations of
        [] ->
            errors

        ( comparable, validation ) :: t ->
            case validation of
                ValidationSuccess ->
                    accValidationDict errors t

                ValidationFailure err ->
                    accValidationDict (( comparable, err ) :: errors) t



-- Validate


type alias Validate a =
    a -> Bool


bindValidate : Validate a -> Validate a -> Validate a
bindValidate test1 test2 =
    \a -> test1 a && test2 a


bindValidates : Nonempty (Validate a) -> Validate a
bindValidates tests =
    bindValidates_ (NE.head tests) (NE.tail tests)


bindValidates_ : Validate a -> List (Validate a) -> Validate a
bindValidates_ test tests =
    case tests of
        [] ->
            test

        h :: t ->
            bindValidates_ (bindValidate test h) t



-- Helpers
-- Turn them public ?


hasError : List (Validation err) -> Maybe (List err)
hasError validations =
    case accValidation [] validations of
        [] ->
            Nothing

        errors ->
            Just errors


isValid : List (Validation err) -> Bool
isValid validations =
    case accValidation [] validations of
        [] ->
            True

        _ ->
            False
