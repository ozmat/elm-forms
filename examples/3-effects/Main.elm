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
    { myForm : F.Form String () OtherModel
    , effectValue : Int
    }



{- Form -}


type alias OtherModel =
    { fieldWatched : String
    , fieldNotWatched : String
    }


myFormFields : FF.Fields String
myFormFields =
    FF.fields
        [ ( "field_watched", FF.input )
        , ( "field_not_watched", FF.input )
        ]


myFormValidate : FV.Validate String () OtherModel
myFormValidate fields =
    FV.valid OtherModel
        |> FV.required fields "field_watched" (FV.stringField <| FV.success)
        |> FV.required fields "field_not_watched" (FV.stringField <| FV.success)



{- Msg -}


type Msg
    = Form (FU.Msg String)
    | EffectSuccess Int



{- Update -}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Form formMsg ->
            let
                newModel =
                    { model | myForm = FU.updateForm formMsg model.myForm }

                console =
                    log "" (F.validate newModel.myForm)
            in
            FU.stringFieldCommands model formMsg myFormCommands

        EffectSuccess newEffectValue ->
            { model | effectValue = newEffectValue } ! []


myFormCommands : Model -> String -> String -> ( Model, Cmd Msg )
myFormCommands model key value =
    case key of
        "field_watched" ->
            ( model
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
        [ inputText True (toString model.effectValue) ""
        , inputText False "Watched" "field_watched"
        , inputText False "Not Watched" "field_not_watched"
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
                , placeholder ("Current effect value : " ++ placeHolder)
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
