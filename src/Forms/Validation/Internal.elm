module Forms.Validation.Internal
    exposing
        ( FieldError(..)
        , FieldValidation
        , FormError(..)
        , FormValidation
        , Validate
        , configFailure
        , fieldValid
        , fieldsValid
        , groupValid
        , toFormResult
        )

import Dict
import Forms.Field.Internal as IF exposing (Fields)
import Forms.Validation.Result exposing (ConfigError(..), FormResult(..))
import Forms.Value.Internal as IV exposing (Value)
import Validation as VA exposing (Validation)


-- Field validation


{-| A `FieldError` represents an error that happened during a `FieldValidation` :

    CustomErr yourError   -- Your type of error
    ConfigErr configError -- Configuration error on the `Field`

-}
type FieldError err
    = CustomErr err
    | ConfigErr ConfigError


{-| A `FieldValidation` represents the [`Validation`](http://package.elm-lang.org/packages/ozmat/elm-validation/latest/Validation#Validation) of a `Field`
-}
type alias FieldValidation err a =
    Validation (FieldError err) a


{-| Returns a failed `FieldValidation` using a `ConfigError`
-}
configFailure : ConfigError -> FieldValidation err a
configFailure err =
    VA.failure (ConfigErr err)



-- Form validation


{-| A `FormError` represents an error that happened during a `FormValidation`.
It's basically a wrapper around the `FieldError` with the key associated
to the `Field`
-}
type FormError comparable err
    = FormError comparable (FieldError err)


{-| Converts a `FormError` into a Tuple
-}
toTuple : FormError comparable err -> ( comparable, FieldError err )
toTuple (FormError comparable fe) =
    ( comparable, fe )


{-| A `FormValidation` represents the [`Validation`](http://package.elm-lang.org/packages/ozmat/elm-validation/latest/Validation#Validation) of a `Form`
-}
type alias FormValidation comparable err a =
    VA.Validation (FormError comparable err) a


{-| Turns a `FieldValidation` into a `FormValidation`
-}
mapFormError : comparable -> FieldValidation err a -> FormValidation comparable err a
mapFormError comparable fv =
    VA.mapError (FormError comparable) fv



-- Form Result


{-| Converts `FormError`s into `Tuple`s and returns the `CustomErrors` if
there are no `ConfigError`s. Returns the `ConfigError`s otherwise.
-}
filterErrors : List (FormError comparable err) -> Result (List ( comparable, ConfigError )) (List ( comparable, err ))
filterErrors errors =
    let
        walk ( comparable, fe ) acc =
            case acc of
                Ok l ->
                    case fe of
                        ConfigErr err ->
                            Err [ ( comparable, err ) ]

                        CustomErr err ->
                            Ok (( comparable, err ) :: l)

                Err l ->
                    case fe of
                        ConfigErr err ->
                            Err (( comparable, err ) :: l)

                        _ ->
                            acc
    in
    List.foldl walk (Ok []) (List.map toTuple errors)


{-| Converts a `FormValidation` into a `FormResult`
-}
toFormResult : FormValidation comparable err a -> FormResult comparable err a
toFormResult formv =
    case VA.toResult formv of
        Ok a ->
            Valid a

        Err fe ->
            case filterErrors fe of
                Ok ce ->
                    Invalid (Dict.fromList ce)

                Err ce ->
                    Error (Dict.fromList ce)



-- Validate a Form


{-| `Validate` represents a function that validates a `Form`.
It takes a group of `Field`s and returns a `FormValidation`
-}
type alias Validate comparable err a =
    Fields comparable -> FormValidation comparable err a



-- One field


fieldValid : Fields comparable -> comparable -> (Value -> FieldValidation err a) -> FormValidation comparable err a
fieldValid fields comparable fvalid =
    let
        missing mvalue =
            case mvalue of
                Nothing ->
                    configFailure MissingField

                Just value ->
                    fvalid value
    in
    mapFormError comparable (missing (IF.getValue comparable fields))



-- Two Fields
-- TODO implement a generic version (x fields) if this feature is used
-- TODO make sure we want to fail on both fields


fieldsValid : Fields comparable -> comparable -> comparable -> (Value -> Value -> FieldValidation err a) -> FormValidation comparable err a
fieldsValid fields comparable1 comparable2 fvalid =
    let
        missing =
            ConfigErr MissingField

        fe1 =
            FormError comparable1

        fe2 =
            FormError comparable2

        both err =
            [ fe1 err, fe2 err ]

        replace ve =
            case ve of
                VA.Error e ->
                    VA.ErrorList (both e)

                VA.ErrorList l ->
                    VA.ErrorList (List.map fe1 l ++ List.map fe2 l)
    in
    case ( IF.getValue comparable1 fields, IF.getValue comparable2 fields ) of
        ( Nothing, Just _ ) ->
            VA.failure (fe1 missing)

        ( Just _, Nothing ) ->
            VA.failure (fe2 missing)

        ( Nothing, Nothing ) ->
            VA.failureWithList (both missing)

        ( Just value1, Just value2 ) ->
            VA.mapValidationError replace (fvalid value1 value2)



-- FieldGroup


groupValid : Fields comparable -> comparable -> (Fields comparable -> FormValidation comparable err a) -> FormValidation comparable err a
groupValid fields comparable fvalid =
    let
        missing mgroup =
            case mgroup of
                Nothing ->
                    mapFormError comparable (configFailure MissingField)

                Just value ->
                    fvalid value
    in
    missing (IF.getGroup comparable fields)
