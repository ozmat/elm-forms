module Forms.Update
    exposing
        ( Msg(..)
        , boolFieldCommands
        , formCommands
        , stringFieldCommands
        , updateForm
        )

{-| This module provides helpers for your update function. Please refer to the
[examples](https://github.com/ozmat/elm-forms/tree/master/examples) for a better understanding


# Messages

@docs Msg


# Update

@docs updateForm


# Side effects

@docs formCommands, stringFieldCommands, boolFieldCommands

-}

import Forms as F exposing (Form(..))
import Forms.Field.Internal as IF
import Forms.Value as V


-- Messages


{-| Predifined messages to help updating a `Field`. `UpdateStringField` is
for `String` `Field`s (input, select) and `UpdateBoolField` is
for `Bool` `Field`s (checkbox)
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
            -- select/input field
            onEvent (Form << Form.Update.UpdateStringField fieldKey)
            -- checkbox field
            onEvent (Form << Form.Update.UpdateBoolField fieldKey)
        ...

-}
updateForm : Msg comparable -> Form comparable err a -> Form comparable err a
updateForm msg (Form fields validate) =
    case msg of
        UpdateStringField comparable s ->
            Form (IF.setValue comparable (V.string s) fields) validate

        UpdateBoolField comparable b ->
            Form (IF.setValue comparable (V.bool b) fields) validate



-- Side effects


{-| Helps defining side-effects on a `Field` event. You can use this
function to run commands when a specific event is triggered on a `Field`.

    addCommand              -- (model, Cmd YourMsg)
        model
        formMsg
        stringFieldCommands
        boolFieldCommands

-}
formCommands :
    model
    -> Msg comparable
    -> (model -> comparable -> String -> ( model, Cmd msg ))
    -> (model -> comparable -> Bool -> ( model, Cmd msg ))
    -> ( model, Cmd msg )
formCommands model msg seffects beffects =
    case msg of
        UpdateStringField comparable s ->
            seffects model comparable s

        UpdateBoolField comparable b ->
            beffects model comparable b


{-| This version only defines side-effects for the `UpdateStringField` message
-}
stringFieldCommands :
    model
    -> Msg comparable
    -> (model -> comparable -> String -> ( model, Cmd msg ))
    -> ( model, Cmd msg )
stringFieldCommands model msg seffects =
    formCommands model msg seffects (\_ _ _ -> ( model, Cmd.none ))


{-| This version only defines side-effects for the `UpdateBoolField` message
-}
boolFieldCommands :
    model
    -> Msg comparable
    -> (model -> comparable -> Bool -> ( model, Cmd msg ))
    -> ( model, Cmd msg )
boolFieldCommands model msg beffects =
    formCommands model msg (\_ _ _ -> ( model, Cmd.none )) beffects
