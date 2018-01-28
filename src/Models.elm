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



-- Maybe public ?
-- hasError : List (Validation err) -> Maybe (List err)
-- hasError validations =
--     case accValid [] validations of
--         [] ->
--             Nothing
--         errors ->
--             Just errors


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


type alias FieldName =
    String


type alias Field err =
    { name : FieldName
    , value : Value
    , fieldType : FieldType
    , validator : Validator err
    }


mkField : FieldName -> Value -> FieldType -> Validator err -> Field err
mkField =
    Field


updateField : Value -> Maybe (Field err) -> Maybe (Field err)
updateField newValue field =
    case field of
        Nothing ->
            Nothing

        Just f ->
            Just { f | value = newValue }


type alias Form err =
    { fields : Dict.Dict FieldName (Field err)
    }


updateFields : FieldName -> Value -> Form err -> Form err
updateFields name value form =
    { form | fields = Dict.update name (updateField value) form.fields }


mkForm : List (Field err) -> Form err
mkForm fields =
    List.foldl (\field dict -> Dict.insert field.name field dict) Dict.empty fields
        |> Form



-- With setter : replace
-- { model | form = updateForm formMsg model.form }


withSetter : a -> Form err -> (Form err -> a -> a) -> FormMsg -> a
withSetter model form setter msg =
    setter (updateForm msg form) model



-- MSG


type FormMsg
    = UpdateStrField FieldName String
    | UpdateBooleanField FieldName Bool


formMsg : (FormMsg -> msg) -> (a -> FormMsg) -> a -> msg
formMsg parentMsg partialMsg a =
    parentMsg (partialMsg a)



-- Wrap MSG ?
-- UPDATE


updateForm : FormMsg -> Form err -> Form err
updateForm msg form =
    case msg of
        UpdateStrField name s ->
            updateFields name (Str s) form

        UpdateBooleanField name b ->
            updateFields name (Boolean b) form
