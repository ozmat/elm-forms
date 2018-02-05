module Forms.Update
    exposing
        ( Msg(..)
        , updateForm
          -- Side effects
        , effects
        , effectsS
        , effectsB
        )

{-| This module provides helpers for your update function. Please refer to the
[basic examples]() (or [advanced examples]()) for a better understanding


# Message

@docs Msg


# Update

@docs updateForm


# Side effects

@docs effects, effectsS, effectsB

-}

import Forms.Form as F exposing (Form(..))
import Forms.Field as FI exposing (setValue)
import Forms.Value as V exposing (string, bool)


-- Messages


{-| Predifined messages to help updating a `Field`. `UpdateStringField` is for
`String` `Value` `Field` and `UpdateBoolField` is for `Bool` `Value` `Field`
-}
type Msg comparable
    = UpdateStringField comparable String
    | UpdateBoolField comparable Bool



-- Update


{-| This function will update a `Form` for you. Here is how to use it :

    type YourMsg
        = Form (Form.Update.Msg comparable)
        | ....

    yourUpdate : YourMsg -> Model -> Model
    yourUpdate msg model =
        case msg of
            Form formMsg ->
                { model | yourForm = Form.Update.updateForm formMsg model.yourForm }

    yourView : Model -> Html YourMsg
    yourView model =
        ...
            onEvent (Form << Form.Update.UpdateStringField fieldKey)
            onEvent (Form << Form.Update.UpdateBoolField fieldKey)
        ...

-}
updateForm : Msg comparable -> Form comparable err a -> Form comparable err a
updateForm msg (Form fields validate) =
    case msg of
        UpdateStringField comparable s ->
            Form (setValue comparable (string s) fields) validate

        UpdateBoolField comparable b ->
            Form (setValue comparable (bool b) fields) validate



-- Side effects


{-| Helps defining some extra effects on a `Field` event. You can use this
function to add your effects when a specific event is triggered on a `Field`.

    effects model formMsg stringEffects boolEffects -- Cmd YourMsg

-}
effects : a -> Msg comparable -> (a -> comparable -> String -> Cmd msg) -> (a -> comparable -> Bool -> Cmd msg) -> Cmd msg
effects model msg stringEffects boolEffects =
    case msg of
        UpdateStringField comparable s ->
            stringEffects model comparable s

        UpdateBoolField comparable b ->
            boolEffects model comparable b


{-| This version only defines effects for the `UpdateStringField` message
-}
effectsS : a -> Msg comparable -> (a -> comparable -> String -> Cmd msg) -> Cmd msg
effectsS model msg stringEffects =
    effects model msg stringEffects (\_ _ _ -> Cmd.none)


{-| This version only defines effects for the `UpdateBoolField` message
-}
effectsB : a -> Msg comparable -> (a -> comparable -> Bool -> Cmd msg) -> Cmd msg
effectsB model msg boolEffects =
    effects model msg (\_ _ _ -> Cmd.none) boolEffects
