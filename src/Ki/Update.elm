module Ki.Update exposing (..)

import Ki.Form as F exposing (Form, setFields)
import Ki.Field as FI exposing (setValue)
import Ki.Value as V exposing (string, bool)


-- TODO add helper to lift MSG in view ?
-- MSG


type Msg comparable
    = UpdateStrField comparable String
    | UpdateBooleanField comparable Bool



-- Update


updateForm : Msg comparable -> Form comparable err a -> Form comparable err a
updateForm msg form =
    case msg of
        UpdateStrField comparable s ->
            setFields (setValue comparable (string s) form.fields) form

        UpdateBooleanField comparable b ->
            setFields (setValue comparable (bool b) form.fields) form



-- Implement this in your app update :
-- ```{ model | form = updateForm formMsg model.form }```
-- Or use this if you have a setter :
-- ``` ```


withSetter : a -> Form comparable err a -> (Form comparable err a -> a -> a) -> Msg comparable -> a
withSetter model form setter msg =
    setter (updateForm msg form) model
