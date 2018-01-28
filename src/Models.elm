module Models exposing (..)

import Dict
import List.Nonempty as NE exposing (Nonempty, (:::))


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



-- Validation


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


accValid : List err -> List (Validation err) -> List err
accValid errors validations =
    case validations of
        [] ->
            errors

        h :: t ->
            case h of
                ValidationSuccess ->
                    accValid errors t

                ValidationFailure err ->
                    accValid (err :: errors) t


accValidDict : List ( comparable, List err ) -> List ( comparable, Validation (List err) ) -> List ( comparable, List err )
accValidDict errors validations =
    case validations of
        [] ->
            errors

        ( comparable, valid ) :: t ->
            case valid of
                ValidationSuccess ->
                    accValidDict errors t

                ValidationFailure err ->
                    accValidDict (( comparable, err ) :: errors) t



-- Maybe public ?
-- hasError : List (Validation err) -> Maybe (List err)
-- hasError validations =
--     case accValid [] validations of
--         [] ->
--             Nothing
--         errors ->
--             Just errors
-- isValid : List (Validation err) -> Bool
-- isValid validations =
--     case accValid [] validations of
--         [] ->
--             True
--         _ ->
--             False


type Validator err
    = NoValidation
    | Validator (Nonempty ( err, Validate Value ))


validator : Value -> Validator err -> Validation (List err)
validator value v =
    case v of
        NoValidation ->
            ValidationSuccess

        Validator vs ->
            let
                es =
                    NE.map (uncurry (validate value)) vs
            in
                case accValid [] (NE.toList es) of
                    [] ->
                        ValidationSuccess

                    ess ->
                        ValidationFailure ess



-- Field types


type FieldType
    = Optional
    | Required


type alias Field err =
    { value : Value
    , fieldType : FieldType
    , validator : Validator err
    }


fieldValidation : Field err -> Validation (List err)
fieldValidation field =
    validator field.value field.validator


mkField : comparable -> Value -> FieldType -> Validator err -> ( comparable, Field err )
mkField comparable value fieldType valid =
    ( comparable, Field value fieldType valid )


updateField : Value -> Maybe (Field err) -> Maybe (Field err)
updateField newValue field =
    case field of
        Nothing ->
            Nothing

        Just f ->
            Just { f | value = newValue }


type alias Form comparable err =
    { fields : Dict.Dict comparable (Field err)
    }


formValidation : Form comparable err -> Validation (Dict.Dict comparable (List err))
formValidation form =
    let
        -- fieldValids : List (comparable, Validation (List err))
        fieldValids =
            List.map (Tuple.mapSecond fieldValidation) (Dict.toList form.fields)
    in
        case accValidDict [] fieldValids of
            [] ->
                ValidationSuccess

            errs ->
                ValidationFailure (Dict.fromList errs)


updateFields : comparable -> Value -> Form comparable err -> Form comparable err
updateFields comparable value form =
    { form | fields = Dict.update comparable (updateField value) form.fields }


mkForm : List ( comparable, Field err ) -> Form comparable err
mkForm fields =
    List.foldl (\( comparable, field ) dict -> Dict.insert comparable field dict) Dict.empty fields
        |> Form



-- With setter : replace
-- { model | form = updateForm formMsg model.form }


withSetter : a -> Form comparable err -> (Form comparable err -> a -> a) -> FormMsg comparable -> a
withSetter model form setter msg =
    setter (updateForm msg form) model



-- MSG


type FormMsg comparable
    = UpdateStrField comparable String
    | UpdateBooleanField comparable Bool


formMsg : (FormMsg comparable -> msg) -> (a -> FormMsg comparable) -> a -> msg
formMsg parentMsg partialMsg a =
    parentMsg (partialMsg a)



-- Wrap MSG ?
-- UPDATE


updateForm : FormMsg comparable -> Form comparable err -> Form comparable err
updateForm msg form =
    case msg of
        UpdateStrField name s ->
            updateFields name (Str s) form

        UpdateBooleanField name b ->
            updateFields name (Boolean b) form
