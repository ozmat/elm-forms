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


foldValid : List err -> List (Validation err) -> List err
foldValid errors validations =
    case validations of
        [] ->
            errors

        h :: t ->
            case h of
                ValidationSuccess ->
                    foldValid errors t

                ValidationFailure err ->
                    foldValid (err :: errors) t



-- Maybe public ?
-- hasError : List (Validation err) -> Maybe (List err)
-- hasError validations =
--     case foldValid [] validations of
--         [] ->
--             Nothing
--         errors ->
--             Just errors


type Validator
    = NoValidation
    | Validator (Nonempty (Validate Value))


validator : Value -> Validator -> Nonempty err -> Validation (List err)
validator value v errors =
    case v of
        NoValidation ->
            ValidationSuccess

        Validator vs ->
            let
                es =
                    NE.map (uncurry (validate value)) (NE.zip errors vs)
            in
                case foldValid [] (NE.toList es) of
                    [] ->
                        ValidationSuccess

                    ess ->
                        ValidationFailure ess


type FieldType
    = Optional
    | Required


type alias FieldName =
    String


type alias Field =
    { name : FieldName
    , value : Value
    , fieldType : FieldType
    , validator : Validator
    }


mkField : FieldName -> Value -> FieldType -> Validator -> Field
mkField =
    Field


updateField : Value -> Maybe Field -> Maybe Field
updateField newValue field =
    case field of
        Nothing ->
            Nothing

        Just f ->
            Just { f | value = newValue }


type alias Form =
    { fields : Dict.Dict FieldName Field
    }


updateFields : FieldName -> Value -> Form -> Form
updateFields name value form =
    { form | fields = Dict.update name (updateField value) form.fields }


mkForm : List Field -> Form
mkForm fields =
    List.foldl (\field dict -> Dict.insert field.name field dict) Dict.empty fields
        |> Form



-- With setter : replace
-- { model | form = updateForm formMsg model.form }


withSetter : a -> Form -> (Form -> a -> a) -> FormMsg -> a
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


updateForm : FormMsg -> Form -> Form
updateForm msg form =
    case msg of
        UpdateStrField name s ->
            updateFields name (Str s) form

        UpdateBooleanField name b ->
            updateFields name (Boolean b) form
