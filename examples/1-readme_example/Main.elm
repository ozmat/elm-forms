module Main exposing (..)

import Debug exposing (log)
import Forms.Field as FF
import Forms.Form as F
import Forms.Update as FU
import Forms.Validation as FV
import Html exposing (Html, div, input, program, text)
import Html.Attributes exposing (placeholder, style)
import Html.Events exposing (onInput)


{- MAIN -}


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }



{- Model -}


init : ( Model, Cmd Msg )
init =
    ( -- Form creation using the form `Fields` and the `Validate` function
      Model (F.form myFormFields myFormValidate)
    , Cmd.none
    )


type alias Model =
    { -- Store the Form state. By looking at the `Form` type we can already see
      -- that the field keys are `String`,
      -- the form errors are `MyFormError`
      -- and the successful `FormResult` is an `OtherModel`
      myForm : F.Form String MyFormError OtherModel
    }



{- Form -}


type alias OtherModel =
    { firstName : String
    , lastName : String
    , referenceNumber : Int
    }


type MyFormError
    = EmptyString
    | NotInt


myFormFields : FF.Fields String
myFormFields =
    -- Here we define the form `Fields` by specifying the form structure
    -- and the key/type of each `Field` (input, select or checkbox).
    -- You can also specify a default value using the `WithDefault` helpers
    FF.fields
        [ ( "first-name", FF.input )
        , ( "last-name", FF.input )
        , ( "reference-number", FF.input )
        ]


myFormValidate : FV.Validate String MyFormError OtherModel
myFormValidate fields =
    -- Here we define the `Validate` function that takes care of the
    -- form validation process. Notice that the function has a `fields`
    -- parameter and also that we respect a specific order when defining
    -- the function (order of the `OtherModel` records).
    --
    -- So to get a valid `OtherModel`
    FV.valid OtherModel
        -- We require in our fields a field named "first-name"
        -- that is a `stringField` (== input or select)
        -- and is not empty otherwise we should have an `EmptyString` error
        |> FV.required fields "first-name" (FV.stringField <| FV.notEmpty EmptyString FV.success)
        -- Same than the previous one with "last-name"
        |> FV.required fields "last-name" (FV.stringField <| FV.notEmpty EmptyString FV.success)
        -- Finally we require a "reference-number"
        -- That is a `stringField` and can be cast into an `Int`
        -- otherwise we should have a `NotInt` error
        |> FV.required fields "reference-number" (FV.stringField <| FV.int NotInt FV.success)



{- Msg -}


type Msg
    = -- Define the form messages in your messages.
      -- You'll want to name your message according to the form it's handling
      -- in order to avoid confusion when using multiple forms.
      -- E.g : one 'RegisterForm' message, one 'LoginForm' message etc.
      Form (FU.Msg String)



{- Update -}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Each time we get a form message
        Form formMsg ->
            let
                -- We update our `Form` (with the new field value)
                newModel =
                    { model | myForm = FU.updateForm formMsg model.myForm }

                -- We also log the result of the form validation in order to
                -- see the current state of the form (debug).
                -- You'll want to validate the form on each form message
                -- if you want a "live form validation" effect.
                -- Otherwise you'll want to validate the form only when
                -- the user hits the submit button
                console =
                    log "" (F.validate newModel.myForm)
            in
            newModel ! []



{- View -}


view : Model -> Html Msg
view model =
    div []
        [ -- Display our Form Fields
          inputText "First Name" "first-name"
        , inputText "Last Name" "last-name"
        , inputText "Reference" "reference-number"
        ]


inputText : String -> String -> Html Msg
inputText placeHolder fieldName =
    let
        inputStyle =
            style
                [ ( "width", "100%" )
                , ( "height", "40px" )
                , ( "padding", "10px 0" )
                , ( "font-size", "2em" )
                , ( "text-align", "center" )
                ]
    in
    input
        [ inputStyle
        , placeholder placeHolder

        -- Here we define the form message to send when the field changes
        -- (`stringFieldMsg` for input/select and `boolFieldMsg` for checkbox)
        , onInput (FU.stringFieldMsg Form fieldName)
        ]
        []
