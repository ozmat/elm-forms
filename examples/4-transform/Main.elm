module Main exposing (..)

import Dict exposing (Dict)
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
    { isRed : Bool
    , floatNumber : Float
    , repeated : Dict String Int
    }


myFormFields : FF.Fields String
myFormFields =
    FF.fields
        [ ( "red", FF.input )
        , ( "float", FF.input )
        , ( "repeat_group"
          , FF.group
                [ ( "repeat1", FF.input )
                , ( "repeat2", FF.input )
                , ( "repeat3", FF.input )
                , ( "repeat4", FF.input )
                , ( "repeat5", FF.input )
                ]
          )
        ]


myFormValidate : FV.Validate String () OtherModel
myFormValidate fields =
    FV.valid OtherModel
        -- Transform String to Bool
        |> FV.required fields "red" (FV.stringValid <| FV.success << ((==) "red"))
        -- Transform String to Float
        |> FV.required fields "float" (FV.stringValid <| FV.int <| FV.success << (*) 5.4 << toFloat)
        -- Transform 5 Strings to  Dict ( String, Int )
        |> FV.fieldGroup fields "repeat_group" repeatValidate


repeatCount : String -> String -> String -> String -> String -> Dict String Int
repeatCount s1 s2 s3 s4 s5 =
    let
        update m =
            case m of
                Just n ->
                    Just (n + 1)

                Nothing ->
                    Just 1

        fold n acc =
            Dict.update n update acc
    in
        List.foldl fold Dict.empty [ s1, s2, s3, s4, s5 ]


repeatValidate : FV.Validate String () (Dict String Int)
repeatValidate fields =
    FV.valid repeatCount
        |> FV.required fields "repeat1" (FV.stringValid <| FV.success << String.toLower)
        |> FV.required fields "repeat2" (FV.stringValid <| FV.success << String.toLower)
        |> FV.required fields "repeat3" (FV.stringValid <| FV.success << String.toLower)
        |> FV.required fields "repeat4" (FV.stringValid <| FV.success << String.toLower)
        |> FV.required fields "repeat5" (FV.stringValid <| FV.success << String.toLower)



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
        [ inputText "Red" "red"
        , inputText "Float" "float"
        , inputText "Repeat 1" "repeat1"
        , inputText "Repeat 2" "repeat2"
        , inputText "Repeat 3" "repeat3"
        , inputText "Repeat 4" "repeat4"
        , inputText "Repeat 5" "repeat5"
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
