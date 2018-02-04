module Forms.Update
    exposing
        ( Msg(..)
        , updateForm
        , withSetter
          -- Side effects
        , effects
        , effectsS
        , effectsB
        )

import Forms.Form as F exposing (Form(..))
import Forms.Field as FI exposing (setValue)
import Forms.Value as V exposing (string, bool)


-- MSG


type Msg comparable
    = UpdateStringField comparable String
    | UpdateBoolField comparable Bool



-- Update


updateForm : Msg comparable -> Form comparable err a -> Form comparable err a
updateForm msg (Form fields validate) =
    case msg of
        UpdateStringField comparable s ->
            Form (setValue comparable (string s) fields) validate

        UpdateBoolField comparable b ->
            Form (setValue comparable (bool b) fields) validate



-- Implement this in your app update :
-- ```{ model | form = updateForm formMsg model.form }```
-- Or use this if you have a setter :
-- ``` ```


withSetter : a -> Form comparable err a -> (Form comparable err a -> a -> a) -> Msg comparable -> a
withSetter model form setter msg =
    setter (updateForm msg form) model



-- Side effects


effects : a -> Msg comparable -> (a -> comparable -> String -> Cmd msg) -> (a -> comparable -> Bool -> Cmd msg) -> Cmd msg
effects model msg stringEffects boolEffects =
    case msg of
        UpdateStringField comparable s ->
            stringEffects model comparable s

        UpdateBoolField comparable b ->
            boolEffects model comparable b


effectsS : a -> Msg comparable -> (a -> comparable -> String -> Cmd msg) -> Cmd msg
effectsS model msg stringEffects =
    effects model msg stringEffects (\_ _ _ -> Cmd.none)


effectsB : a -> Msg comparable -> (a -> comparable -> Bool -> Cmd msg) -> Cmd msg
effectsB model msg boolEffects =
    effects model msg (\_ _ _ -> Cmd.none) boolEffects