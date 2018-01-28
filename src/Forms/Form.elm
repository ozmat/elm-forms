module Forms.Form
    exposing
        ( Form
        , mkForm
        , updateFields
        , formValidate
        )

import Dict
import Forms.Field exposing (Field, fieldValidate, updateValue)
import Forms.Validation exposing (Validation(..), accValidationDict)
import Forms.Value exposing (Value)


type alias Form comparable err =
    { fields : Dict.Dict comparable (Field err)
    }


mkForm : List ( comparable, Field err ) -> Form comparable err
mkForm fields =
    Form (Dict.fromList fields)


updateFields : comparable -> Value -> Form comparable err -> Form comparable err
updateFields comparable value form =
    { form | fields = Dict.update comparable (updateValue value) form.fields }



-- Validation


formValidate : Form comparable err -> Validation (Dict.Dict comparable (List err))
formValidate form =
    let
        -- fieldValids : List (comparable, Validation (List err))
        fieldValidations =
            List.map (Tuple.mapSecond fieldValidate) (Dict.toList form.fields)
    in
        case accValidationDict [] fieldValidations of
            [] ->
                ValidationSuccess

            fieldErrors ->
                ValidationFailure (Dict.fromList fieldErrors)
