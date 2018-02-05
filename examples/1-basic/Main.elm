module Main exposing (..)

import Html exposing (Html, program, text, div, input)
import Html.Attributes exposing (placeholder, style)
import Html.Events exposing (onInput)
import Debug exposing (log)


-- Forms import

import Forms.Field as FF
import Forms.Validation as FV
import Forms.Form as F
import Forms.Update as FU


{- MAIN -}


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , subscriptions = (\_ -> Sub.none)
        , view = view
        }



{- Model -}


init : ( Model, Cmd Msg )
init =
    ( Model (F.form myFormFields myFormValidate)
    , Cmd.none
    )


type alias Model =
    { myForm : F.Form String () OtherModel
    }



{- Form -}


type alias OtherModel =
    { firstName : String
    , lastName : String
    , referenceNumber : Int
    }


myFormFields : FF.Group String
myFormFields =
    FF.fields
        [ ( "first_name", FF.string )
        , ( "last_name", FF.string )
        , ( "reference_number", FF.string )
        ]


myFormValidate : FV.Validate String () OtherModel
myFormValidate fields =
    FV.valid OtherModel
        |> FV.required fields "first_name" (FV.stringValid <| FV.notEmpty <| FV.success)
        |> FV.required fields "last_name" (FV.stringValid <| FV.notEmpty <| FV.success)
        |> FV.required fields "reference_number" (FV.stringValid <| FV.int <| FV.success)



{- Msg -}


type Msg
    = Form (FU.Msg String)



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
                newModel ! []



{- View -}


view : Model -> Html Msg
view model =
    div []
        [ inputText "First Name" "first_name"
        , inputText "Last Name" "last_name"
        , inputText "Reference" "reference_number"
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
            , onInput (Form << FU.UpdateStringField fieldName)
            ]
            []