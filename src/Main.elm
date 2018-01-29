module Main exposing (..)

import Html exposing (Html, Attribute, beginnerProgram, text, div, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Debug exposing (log)
import Dict as D


-- Can't reexport ...

import Forms.Field exposing (..)
import Forms.Form exposing (..)
import Forms.Update exposing (..)
import Forms.Validation exposing (..)
import Forms.Value exposing (..)
import Ki.Value as V exposing (..)
import Ki.Field as F exposing (..)
import Ki.Validation as VA exposing (..)


-- MAIN


main =
    beginnerProgram
        { model = initModel
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { form : Form String String
    }


someForm : Form String String
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
    = Form (FormMsg String)



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        Form formMsg ->
            let
                newModel =
                    { model | form = updateForm formMsg model.form }

                -- x =
                --     log "" newModel.form
                y =
                    log "" (setValue "test" (V.string "rouge") test2)
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


test : Maybe V.Value
test =
    getValue "test" test2


test2 : F.Group String
test2 =
    (D.fromList
        [ ( "z", F.string "gre" )
        , ( "w", F.string "gre" )
        , ( "r", F.string "gre" )
        , ( "tes", F.string "gre" )
        , ( "group1"
          , (FieldGroup
                (D.fromList
                    [ ( "z", F.string "gre" )
                    , ( "tesdt", F.string "e" )
                    , ( "group2"
                      , (FieldGroup
                            (D.fromList
                                [ ( "z", F.string "gre" )
                                , ( "test", F.string "bleu" )
                                ]
                            )
                        )
                      )
                    ]
                )
            )
          )
        ]
    )


test3 : VA.Validate String String Jean
test3 fields =
    VA.valid Jean
        |> VA.requiredAcc fields "z" (\_ -> VA.customFailure "menfou1")
        |> VA.requiredAcc fields "z" (\_ -> VA.customFailure "menfou2")


type alias Jean =
    { a : String
    , b : String
    }
