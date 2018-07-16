module Forms.Validation.Result
    exposing
        ( ConfigError(..)
        , FormResult(..)
        )

{-| Blabla


# Form Result

@docs FormResult


# Configuration Error

@docs ConfigError

-}

import Dict exposing (Dict)


-- Form Result


{-| A `FormResult` represents the result of a `FormValidation`. There are
3 different states :

    Valid   -- Reprensents a successful validation and holds the form output
    Invalid -- Reprensents a failed validation and holds the custom errors
    Error   -- Represents a misconfigured form and holds the config errors

-}
type FormResult comparable err a
    = Valid a
    | Invalid (Dict comparable err)
    | Error (Dict comparable ConfigError)



-- Configuration Error


{-| A `ConfigError` represents a configuration error on a `Field` :

    MissingField -- When the `Field` cannot be found
    WrongType    -- When the `Field` has a different type of `Value`

-}
type ConfigError
    = MissingField
    | WrongType
