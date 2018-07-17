module Main exposing (..)

import Debug exposing (log)
import Forms as F
import Forms.Field as FF
import Forms.Update as FU
import Forms.Validation as FV
import Html exposing (Html, div, input, program, text)
import Html.Attributes exposing (disabled, placeholder, style)
import Html.Events exposing (onInput)
import Random


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
    ( Model (F.form myFormFields myFormValidate) -1
    , Cmd.none
    )


type alias Model =
    { -- We don't really need errors in this example that's why it's `()`
      myForm : F.Form String () OtherModel

    -- But we need to store a value coming from a side effect
    , sideEffectValue : Int
    }



{- Form -}


type alias OtherModel =
    { fieldWatched : String
    , fieldNotWatched : String
    }


myFormFields : FF.Fields String
myFormFields =
    -- We have two fields : one that is watched and one that isn't
    FF.fields
        [ ( "field-watched", FF.input )
        , ( "field-not-watched", FF.input )
        ]


myFormValidate : FV.Validate String () OtherModel
myFormValidate fields =
    -- We don't really need validation in this example. Let's make it simple
    FV.valid OtherModel
        |> FV.required fields "field-watched" (FV.stringField <| FV.success)
        |> FV.required fields "field-not-watched" (FV.stringField <| FV.success)



{- Msg -}


type Msg
    = -- The form messages
      Form (FU.Msg String)
      -- And the side-effect message
    | EffectSuccess Int



{- Update -}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Form formMsg ->
            let
                -- As usual we update the form
                newModel =
                    { model | myForm = FU.updateForm formMsg model.myForm }

                -- and log the result (debug)
                console =
                    log "" (F.validate newModel.myForm)
            in
            -- But here we're also defining some commands.
            -- We explicitly say that we want commands on `stringField`s
            -- because first we only have `stringField`s in our form,
            -- second we want to define commands/side-effects on the
            -- "field-watched" which is a string field.
            -- Note: You can also define commands on bool fields
            -- (`boolFieldCommands`) and on all type of fields (`formCommands`)
            FU.stringFieldCommands model formMsg myFormCommands

        EffectSuccess newEffectValue ->
            -- Here we have a value coming back from the side-effect
            -- so we update the side-effect value
            { model | sideEffectValue = newEffectValue } ! []


myFormCommands : Model -> String -> String -> ( Model, Cmd Msg )
myFormCommands model key value =
    case key of
        -- As previously mentionned we only care about the "field-watched".
        -- We want to run a side-effect command each time this field is
        -- updated.
        "field-watched" ->
            ( model
              -- We're using `Random` to simulate the side-effect
            , Random.generate EffectSuccess (Random.int 1 15)
            )

        _ ->
            ( model
            , Cmd.none
            )



{- View -}


view : Model -> Html Msg
view model =
    div []
        [ -- Let's display the side-effect value to make sure it actually
          -- changes every time we change the "field-watched" value
          inputText True (toString model.sideEffectValue) ""
        , inputText False "Watched" "field-watched"
        , inputText False "Not Watched" "field-not-watched"
        ]


inputText : Bool -> String -> String -> Html Msg
inputText disable placeHolder fieldName =
    let
        inputStyle =
            style
                [ ( "width", "100%" )
                , ( "height", "40px" )
                , ( "padding", "10px 0" )
                , ( "font-size", "2em" )
                , ( "text-align", "center" )
                ]

        inputAttrs =
            if disable then
                [ inputStyle
                , disabled disable
                , placeholder ("Current side-effect value : " ++ placeHolder)
                ]
            else
                [ inputStyle
                , placeholder placeHolder
                , onInput (Form << FU.UpdateStringField fieldName)
                ]
    in
    input
        inputAttrs
        []