module Forms.Validation
    exposing
        ( Validation(..)
        , Validate
        , validate
        , accValidation
        , accValidationDict
        )


type Validation err
    = ValidationFailure err
    | ValidationSuccess


type alias Validate a =
    a -> Bool


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



-- Helpers public ?


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
