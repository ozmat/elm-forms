module Main exposing (..)

import Html exposing (Html, Attribute, beginnerProgram, text, div, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String
import Models exposing (..)
import Debug exposing (log)


-- MAIN


main =
    beginnerProgram
        { model = initModel
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { form : Form
    }


someForm : Form
someForm =
    mkForm
        [ mkField "username" stringValue Required NoValidation
        , mkField "first_name" stringValue Optional NoValidation
        ]


initModel : Model
initModel =
    Model someForm



-- MSG


type Msg
    = Form FormMsg



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        Form formMsg ->
            let
                newModel =
                    { model | form = updateForm formMsg model.form }

                x =
                    log "" newModel.form
            in
                newModel



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ inputText "User braa" "username"

        -- , div [ myStyle ] [ text (String.reverse model.c) ]
        ]


myStyle : Attribute Msg
myStyle =
    style
        [ ( "width", "100%" )
        , ( "height", "40px" )
        , ( "padding", "10px 0" )
        , ( "font-size", "2em" )
        , ( "text-align", "center" )
        ]


inputText : String -> String -> Html Msg
inputText place fieldName =
    input
        [ placeholder place
        , onInput (formMsg Form (UpdateStrField fieldName))
        , myStyle
        ]
        []
