module Forms.Validation
    exposing
        ( -- Field validation
          FieldError(..)
        , FieldValidation
        , failure
        , customFailure
        , success
        , validation
          -- Basic validation
        , stringValid
        , boolValid
        , int
        , float
        , notEmpty
        , length
        , email
        , passwordMatch
          -- Form validation
        , FormError(..)
        , FormValidation
        , valid
        , toTuple
          -- Validate
        , Validate
        , required
        , required1
        , hardcoded
        , hardcoded1
        , optional
        , optional1
        , optionalMaybe
        , optionalMaybe1
        , twoFields
        , twoFields1
        , fieldGroup
        , fieldGroup1
        )

{-| This module provides the validation logic for the library. Please refer to
the [basic examples]() (or [advanced examples]()) for a better understanding


# Field Validation

@docs FieldError, FieldValidation


### Common Helpers

@docs failure, customFailure, success, validation


### Type validation

@docs stringValid, boolValid


### Basic Validation

@docs int, float, notEmpty, length, email, passwordMatch


# Form Validation

@docs FormError, FormValidation


### Common Helpers

@docs valid, toTuple


# Validate

@docs Validate


### Validate Helpers (accumulate)

Those functions help building a `Validate` function and aim to ease the
`FormValidation`. They will accumulate the different `FormError`s during the
validation process.

@docs required, hardcoded, optional, optionalMaybe, twoFields, fieldGroup


### Validate Helpers (bind)

Those functions are the "binding" equivalent of the previous ones. This means
that the `FormValidation` will fail at the first `FormError` encountered and
won't accumulate the errors. Don't mix those functions with the previous ones

@docs required1, hardcoded1, optional1, optionalMaybe1, twoFields1, fieldGroup1

-}

import Regex
import Validation as VA exposing (Validation(..))
import Forms.Field as F exposing (Group)
import Forms.Value as V exposing (Value)


-- Field validation


{-| A `FieldError` represents an error that happened during a `FieldValidation`.
Here are the main ones :

    MissingField          -- When the `Field` cannot be found
    WrongType             -- When the `Value` has a different type
    CustomError yourError -- Your type of error

-}
type FieldError err
    = CustomError err
    | MissingField
    | WrongType
    | NotInt
    | NotFloat
    | EmptyString
    | WrongLength
    | NotEmail
    | PasswordNotEqual


{-| A `FieldValidation` represents the [`Validation`](http://package.elm-lang.org/packages/ozmat/elm-validation/latest/Validation#Validation) of a `Field`
-}
type alias FieldValidation err a =
    Validation (FieldError err) a



-- Common Helpers


{-| Returns a failed `FieldValidation`
-}
failure : FieldError err -> FieldValidation err a
failure =
    VA.failure


{-| Returns a failed `FieldValidation` using a `CustomError`
-}
customFailure : err -> FieldValidation err a
customFailure err =
    failure (CustomError err)


{-| Returns a successful `FieldValidation`
-}
success : a -> FieldValidation err a
success =
    VA.success


{-| Helps creating a basic `FieldValidation` function

    type YourError
        = MustBeEmpty

    emptyValidation : String -> FieldValidation YourError String
    emptyValidation =
        validation MustBeEmpty String.isEmpty

-}
validation : err -> (a -> Bool) -> a -> FieldValidation err a
validation err valid a =
    VA.validation (CustomError err) valid a



-- Type validation


{-| Helps validating a `String` `Value`. If the `Value` has a different type,
fails with a `WrongType` `FieldError`.

    type YourError
        = MustBeEmpty

    validateEmptyField : Value -> FieldValidation YourError String
    validateEmptyField =
        stringValid (validation MustBeEmpty String.isEmpty)

-}
stringValid : (String -> FieldValidation err a) -> Value -> FieldValidation err a
stringValid valid value =
    case value of
        V.String s ->
            valid s

        _ ->
            failure WrongType


{-| Helps validating a `Bool` `Value`. If the `Value` has a different type,
fails with a `WrongType` `FieldError`.

    type YourError
        = MustBeChecked

    validateCheckedField : Value -> FieldValidation YourError Bool
    validateCheckedField =
        boolValid (validation MustBeChecked ((==) True))

-}
boolValid : (Bool -> FieldValidation err a) -> Value -> FieldValidation err a
boolValid valid value =
    case value of
        V.Bool b ->
            valid b

        _ ->
            failure WrongType



-- Basic validation


{-| Helps validating a `String` that can be cast into an `Int`. If the `String`
cannot be cast, fails with a `NotInt` `FieldError`.

    doYourValidation : Int -> FieldValidation err a

    validateField : Value -> FieldValidation err a
    validateField =
        stringValid (int (doYourValidation))

-}
int : (Int -> FieldValidation err a) -> String -> FieldValidation err a
int valid s =
    case String.toInt s of
        Ok i ->
            valid i

        Err _ ->
            failure NotInt


{-| Helps validating a `String` that can be cast into a `Float`. If the `String`
cannot be cast, fails with a `NotFloat` `FieldError`.

    doYourValidation : Float -> FieldValidation err a

    validateField : Value -> FieldValidation err a
    validateField =
        stringValid (float (doYourValidation))

-}
float : (Float -> FieldValidation err a) -> String -> FieldValidation err a
float valid s =
    case String.toFloat s of
        Ok f ->
            valid f

        Err _ ->
            failure NotFloat


{-| Helps validating a `String` that is not empty. If the `String`is empty,
fails with an `EmptyString` `FieldError`.

    doYourValidation : String -> FieldValidation err a

    validateField : Value -> FieldValidation err a
    validateField =
        stringValid (notEmpty (doYourValidation))

-}
notEmpty : (String -> FieldValidation err a) -> String -> FieldValidation err a
notEmpty valid s =
    if String.isEmpty s then
        failure EmptyString
    else
        valid s


{-| Helps validating a `String` that has a specific length (> low && < high).
If the `String` has a different length, fails with a `WrongLength` `FieldError`.

    doYourValidation : String -> FieldValidation err a

    validateField : Value -> FieldValidation err a
    validateField =
        stringValid (length low high (doYourValidation))

-}
length : Int -> Int -> (String -> FieldValidation err a) -> String -> FieldValidation err a
length low high valid s =
    let
        len =
            String.length s
    in
        if len > low && len < high then
            valid s
        else
            failure WrongLength


{-| Helps validating a `String` that is an email. If the `String`is not an email,
fails with a `NotEmail` `FieldError`.

    doYourValidation : String -> FieldValidation err a

    validateField : Value -> FieldValidation err a
    validateField =
        stringValid (email (doYourValidation))

-}
email : (String -> FieldValidation err a) -> String -> FieldValidation err a
email valid s =
    -- https://developer.mozilla.org/en-US/docs/Web/HTML/Element/Input/email
    if Regex.contains (Regex.regex "^[a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$") s then
        valid s
    else
        failure NotEmail


{-| Helps validating two `String` `Value`s that match. If the `String`s
don't match, fails with a `PasswordNotEqual` `FieldError`.

    doYourValidation : String -> FieldValidation err a

    validateField : Value -> Value -> FieldValidation err a
    validateField =
        passwordMatch (doYourValidation)

-}
passwordMatch : (String -> FieldValidation err a) -> Value -> Value -> FieldValidation err a
passwordMatch valid password passwordAgain =
    case ( password, passwordAgain ) of
        ( V.String s1, V.String s2 ) ->
            if s1 == s2 then
                valid s1
            else
                failure PasswordNotEqual

        _ ->
            failure WrongType



-- Form validation


{-| A `FormError` represents an error that happened during a `FormValidation`.
It's basically a wrapper around the `FieldError` with the key associated
to the `Field`
-}
type FormError comparable err
    = FormError comparable (FieldError err)


{-| A `FormValidation` represents the [`Validation`](http://package.elm-lang.org/packages/ozmat/elm-validation/latest/Validation#Validation) of a `Form`
-}
type alias FormValidation comparable err a =
    VA.Validation (FormError comparable err) a



-- Common Helpers


{-| Returns a successful `FormValidation`
-}
valid : a -> FormValidation comparable err a
valid =
    VA.success


{-| Converts a `FormError` into a Tuple
-}
toTuple : FormError comparable err -> ( comparable, FieldError err )
toTuple (FormError comparable fe) =
    ( comparable, fe )


mapFormError : comparable -> FieldValidation err a -> FormValidation comparable err a
mapFormError comparable fv =
    VA.mapError (FormError comparable) fv



-- Validate a Form


{-| `Validate` represents a function that validates a `Form`.
It takes a `Group` of `Field`s and returns a `FormValidation`
-}
type alias Validate comparable err a =
    Group comparable -> FormValidation comparable err a


fieldValid : Group comparable -> comparable -> (Value -> FieldValidation err a) -> FormValidation comparable err a
fieldValid fields comparable valid =
    let
        missing mvalue =
            case mvalue of
                Nothing ->
                    failure MissingField

                Just value ->
                    valid value
    in
        mapFormError comparable (missing (F.getValue comparable fields))



-- Required


{-| Validates a required `Field`. This is the basic use case and will always
validate the `Field`

    ...
        |> required fields comparable doYourValidation
        ...

-}
required : Group comparable -> comparable -> (Value -> FieldValidation err a) -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
required fields comparable valid fvf =
    VA.andMapAcc (fieldValid fields comparable valid) fvf


{-| Validates a required `Field` (binding)

    ...
        |> required1 fields comparable doYourValidation
        ...

-}
required1 : Group comparable -> comparable -> (Value -> FieldValidation err a) -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
required1 fields comparable valid fvf =
    VA.andMap (fieldValid fields comparable valid) fvf


{-| Hardcodes a value for a `Field`. This is useful when you need to harcode
a specific value in the validation process

    ...
        |> hardcoded fields comparable yourHardcodedValue
        ...

`hardcoded` is equivalent to a specific `required`:

    ...
        |> required fields comparable (\_ -> success yourHardcodedValue)
        ...

-}
hardcoded : Group comparable -> comparable -> a -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
hardcoded fields comparable a fvf =
    required fields comparable (\_ -> success a) fvf


{-| Hardcodes a value for a `Field` (binding)

    ...
        |> hardcoded1 fields comparable yourHardcodedValue
        ...

-}
hardcoded1 : Group comparable -> comparable -> a -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
hardcoded1 fields comparable a fvf =
    required1 fields comparable (\_ -> success a) fvf



-- Optional


optional_ : (String -> FieldValidation err a) -> a -> Value -> FieldValidation err a
optional_ valid default =
    stringValid
        (\s ->
            if String.isEmpty s then
                success default
            else
                valid s
        )


{-| Validates an optional `Field`. This will use the validation function if
the `Field` is not empty or use the default value otherwise. Only works
on `String` `Value`

    ...
        |> optional fields comparable doYourValidation yourDefaultValue
        ...

`optional` is equivalent to a specific `required`:

    ...
        |> required
            fields
            comparable
            (stringValid <|
                \str ->
                    if String.isEmpty str then
                        success yourDefaultValue
                    else
                        doYourValidation str
            )
        ...

-}
optional : Group comparable -> comparable -> (String -> FieldValidation err a) -> a -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
optional fields comparable valid default fvf =
    required fields comparable (optional_ valid default) fvf


{-| Validates an optional `Field` (binding)

    ...
        |> optional1 fields comparable doYourValidation yourDefaultValue
        ...

-}
optional1 : Group comparable -> comparable -> (String -> FieldValidation err a) -> a -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
optional1 fields comparable valid default fvf =
    required1 fields comparable (optional_ valid default) fvf


{-| Validates an optional `Field` with a Maybe. Same logic than with `optional`
but the default value is `Nothing` and the validated value is `Just`

    ...
        |> optionalMaybe fields comparable doYourValidation
        ...

-}
optionalMaybe : Group comparable -> comparable -> (String -> FieldValidation err a) -> FormValidation comparable err (Maybe a -> b) -> FormValidation comparable err b
optionalMaybe fields comparable valid fvf =
    optional fields comparable (\s -> VA.map Just (valid s)) Nothing fvf


{-| Validates an optional `Field` with a Maybe (binding)

    ...
        |> optionalMaybe1 fields comparable doYourValidation
        ...

-}
optionalMaybe1 : Group comparable -> comparable -> (String -> FieldValidation err a) -> FormValidation comparable err (Maybe a -> b) -> FormValidation comparable err b
optionalMaybe1 fields comparable valid fvf =
    optional1 fields comparable (\s -> VA.map Just (valid s)) Nothing fvf



-- TwoFields
-- TODO implement a generic version (x fields) if this feature is used


fieldsValid : Group comparable -> comparable -> comparable -> (Value -> Value -> FieldValidation err a) -> FormValidation comparable err a
fieldsValid fields comparable1 comparable2 valid =
    let
        fe1 =
            FormError comparable1

        fe2 =
            FormError comparable2

        both err =
            VA.ErrorList [ fe1 err, fe2 err ]

        replace ve =
            case ve of
                VA.Error e ->
                    both e

                VA.ErrorList l ->
                    VA.ErrorList (List.map fe1 l ++ List.map fe2 l)
    in
        case ( F.getValue comparable1 fields, F.getValue comparable2 fields ) of
            ( Nothing, Just _ ) ->
                VA.failure (fe1 MissingField)

            ( Just _, Nothing ) ->
                VA.failure (fe2 MissingField)

            ( Nothing, Nothing ) ->
                Failure (both MissingField)

            ( Just value1, Just value2 ) ->
                VA.mapValidationError replace (valid value1 value2)


{-| Validates two `Field`s together. The validation function takes two `Field`
`Value`s and returns one result. It is useful when you need to validate one
`Field` that depends on another, but you only need to store one result

    ...
        |> twoFields fields comparable1 comparable2 doYourValidation
        ...

-}
twoFields : Group comparable -> comparable -> comparable -> (Value -> Value -> FieldValidation err a) -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
twoFields fields comparable1 comparable2 valid fvf =
    VA.andMapAcc (fieldsValid fields comparable1 comparable2 valid) fvf


{-| Validates two `Field`s together (binding)

    ...
        |> twoFields1 fields comparable1 comparable2 doYourValidation
        ...

-}
twoFields1 : Group comparable -> comparable -> comparable -> (Value -> Value -> FieldValidation err a) -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
twoFields1 fields comparable1 comparable2 valid fvf =
    VA.andMap (fieldsValid fields comparable1 comparable2 valid) fvf



-- FieldGroup


groupValid : Group comparable -> comparable -> (Group comparable -> FormValidation comparable err a) -> FormValidation comparable err a
groupValid fields comparable valid =
    let
        missing mgroup =
            case mgroup of
                Nothing ->
                    mapFormError comparable (failure MissingField)

                Just value ->
                    valid value
    in
        missing (F.getGroup comparable fields)


{-| Validates a `Group` of `Field`s. This can be useful in many different cases
but mainly when you need to nest a validation process

     ...
        |> fieldGroup fields comparable doYourValidation
        ...

-}
fieldGroup : Group comparable -> comparable -> (Group comparable -> FormValidation comparable err a) -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
fieldGroup fields comparable valid fvf =
    VA.andMapAcc (groupValid fields comparable valid) fvf


{-| Validates a `Group` of `Field`s (binding)

     ...
        |> fieldGroup fields comparable doYourValidation
        ...

-}
fieldGroup1 : Group comparable -> comparable -> (Group comparable -> FormValidation comparable err a) -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
fieldGroup1 fields comparable valid fvf =
    VA.andMap (groupValid fields comparable valid) fvf
