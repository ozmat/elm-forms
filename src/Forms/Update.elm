module Forms.Update
    exposing
        ( Msg
        , boolFieldCommands
        , boolFieldMsg
        , formCommands
        , stringFieldCommands
        , stringFieldMsg
        , updateForm
        )

{-| This module provides the `update` helpers. Please refer to the
[examples](https://github.com/ozmat/elm-forms/tree/master/examples) for a better understanding


# Form Messages

@docs Msg, stringFieldMsg, boolFieldMsg


# Form Update

@docs updateForm


# Side effects

Sometimes you will need to have more control over a field update/event, that is
what the side-effects helpers are for.

@docs stringFieldCommands, boolFieldCommands, formCommands

-}

import Forms.Field.Internal as IF
import Forms.Form.Internal exposing (Form(..))
import Forms.Update.Internal as Internal exposing (Msg(..))
import Forms.Value as V


-- Form Messages


{-| These are the form `Msg`, they help updating a `Field`.

You need to implement them in your `Msg`, `update` function and use the helpers
in your view events.

`stringFieldMsg` is for string `Field` (input, select) and `boolFieldMsg` is
for bool `Field` (checkbox)

    type YourMsg
        = SomeForm (Forms.Update.Msg comparable)
        | ....

    yourUpdate : YourMsg -> Model -> Model
    yourUpdate msg model =
        case msg of
            SomeForm formMsg ->
                ...

    yourView : Model -> Html YourMsg
    yourView model =
        ...
            -- select/input field
            onEvent (Forms.Update.stringFieldMsg SomeForm fieldKey)
            -- checkbox field
            onEvent (Forms.Update.boolFieldMsg SomeForm fieldKey)
        ...

Note: you will want to name your message according to the form it is handling
in order to avoid confusion when using multiple forms

    type YourMsg
        = RegisterForm (Forms.Update.Msg comparable)
        | LoginForm (Forms.Update.Msg comparable)
        | ...

-}
type alias Msg comparable =
    Internal.Msg comparable


{-| Creates a form message that updates a string `Field`
-}
stringFieldMsg : (Msg comparable -> msg) -> comparable -> (String -> msg)
stringFieldMsg msg key =
    msg << UpdateStringField key


{-| Creates a form message that updates a bool `Field`
-}
boolFieldMsg : (Msg comparable -> msg) -> comparable -> (Bool -> msg)
boolFieldMsg msg key =
    msg << UpdateBoolField key



-- Form Update


{-| This function will update a `Form` for you. Once you have implemented the
form `Msg`, use it to update the `Form`

    yourUpdate : YourMsg -> Model -> Model
    yourUpdate msg model =
        case msg of
            SomeForm formMsg ->
                { model
                    | yourForm = Forms.Update.updateForm formMsg model.yourForm
                }
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


{-| Helps defining `Cmd` on both string and bool `Field` events.

    formCommands
        model
        formMsg
        yourStringFieldCommands
        yourBoolFieldCommands

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


{-| Helps defining `Cmd` on string `Field` events. You can use this function
to run commands when an event is triggered on a specific string `Field`.

    yourUpdate : YourMsg -> Model -> ( Model, Cmd YourMsg )
    yourUpdate msg model =
        case msg of
            SomeForm formMsg ->
                let
                    newModel =
                        { model
                            | yourForm = updateForm formMsg model.yourForm
                        }
                in
                stringFieldCommands newModel formMsg someFormCommands
            ...

    someFormCommands : Model -> String -> String -> ( Model, Cmd YourMsg )
    someFormCommands model key value =
        case key of
            "field-name" ->
                ( changeOrNotTheModel model
                , doSomeCommandsWithTheValue value
                )

            ...

            _ ->
                ( model
                , Cmd.none
                )

Note: this function only defines commands for the `stringFieldMsg`

-}
stringFieldCommands :
    model
    -> Msg comparable
    -> (model -> comparable -> String -> ( model, Cmd msg ))
    -> ( model, Cmd msg )
stringFieldCommands model msg seffects =
    formCommands model msg seffects (\_ _ _ -> ( model, Cmd.none ))


{-| Helps defining `Cmd` on bool `Field` events. You can use this function
to run commands when an event is triggered on a specific bool `Field`.

Note: this function only defines commands for the `boolFieldMsg`

-}
boolFieldCommands :
    model
    -> Msg comparable
    -> (model -> comparable -> Bool -> ( model, Cmd msg ))
    -> ( model, Cmd msg )
boolFieldCommands model msg beffects =
    formCommands model msg (\_ _ _ -> ( model, Cmd.none )) beffects
