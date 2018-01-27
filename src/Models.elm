module Models exposing (..)

import Dict


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


type Validator
    = NoValidation
    | Validator (List Validation)


type Validation err a
    = ValidationFailure err
    | ValidationSuccess a



-- type alias TestValue a =
--     a -> Bool


validate : err -> (a -> Bool) -> a -> Validation err a
validate err test a =
    if test a then
        ValidationSuccess a
    else
        ValidationFailure err



-- sequenceV : List (Validation err a) -> Validation err a
-- sequenceV
-- sequence :: [Maybe a] -> Maybe [a]
-- sequence []           = Just []
-- sequence (Nothing:xs) = Nothing
-- sequence (Just x:xs)  = case sequence xs of
--   Just xs' -> Just (x:xs')
--   _        -> Nothing
--


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
