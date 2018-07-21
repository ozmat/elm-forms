module Forms.Validation.Result
    exposing
        ( ConfigError(..)
        , FormResult(..)
        )

{-| This module provides the validation result


# Form Result

@docs FormResult


# Configuration Error

@docs ConfigError

-}

import Dict exposing (Dict)


-- Form Result


{-| A `FormResult` represents the result of a form validation. There are
3 different states :

  - `Valid` reprensents a successful validation and holds the form result
  - `Invalid` reprensents a failed validation and holds the form errors
  - `Error` represents a misconfigured form and holds the configuration errors

-}
type FormResult comparable err a
    = Valid a
    | Invalid (Dict comparable err)
    | Error (Dict comparable ConfigError)



-- Configuration Error


{-| A `ConfigError` represents a configuration error on a `Field`. It usually
happens when there is an error in the `Fields` or the `Validate` function :

  - `MissingField` when the field cannot be found
  - `WrongType` when the field has a different type of `Value`

Note: if you have trouble debugging those errors have a look at the
[README troubleshooting section](http://package.elm-lang.org/packages/ozmat/elm-forms/latest)

-}
type ConfigError
    = MissingField
    | WrongType
