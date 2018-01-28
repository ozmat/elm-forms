module Forms.Update exposing (..)

import Forms.Form exposing (Form, updateFields)
import Forms.Value exposing (Value(..))


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
