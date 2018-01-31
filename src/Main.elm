module Main exposing (..)

import Html exposing (Html, Attribute, beginnerProgram, text, div, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Debug exposing (log)
import Dict as D


-- Can't reexport ...

import Ki.Value as V exposing (..)
import Ki.Field as F exposing (..)
import Ki.Validation as VA exposing (..)
import Ki.Form as FO
import Ki.Update as U


-- MAIN


main =
    beginnerProgram
        { model = initModel
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { form : FO.Form String String Jean
    }


initModel : Model
initModel =
    Model (FO.form test2 test3)



-- MSG


type Msg
    = Form (U.Msg String)



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        Form formMsg ->
            let
                newModel =
                    { model | form = U.updateForm formMsg model.form }

                y =
                    log "" (FO.validate newModel.form)
            in
                newModel



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ inputText "User braa" "z"
        , inputText "User braa" "w"
        , inputText "User braa" "r"

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
        , onInput (Form << U.UpdateStringField fieldName)
        , myStyle
        ]
        []



-- Form test


test : Maybe V.Value
test =
    getValue "test" test2


test2 : F.Group String
test2 =
    F.fields
        [ ( "z", F.string )
        , ( "w", F.string )
        , ( "r", F.string )
        , ( "group1"
          , F.group
                [ ( "zz", F.string )
                , ( "test", F.string )
                , ( "group2"
                  , F.group
                        [ ( "z", F.string )
                        , ( "test", F.string )
                        ]
                  )
                ]
          )
        ]


type alias Jean =
    { a : String
    , b : String
    , c : Maybe String
    , d : Jule
    }


type alias Jule =
    { a : String
    , b : String
    }


test3 : VA.Validate String String Jean
test3 fields =
    VA.valid Jean
        |> VA.requiredAcc fields
            "z"
            (VA.stringField
                (\s ->
                    if String.length s > 2 then
                        VA.validF s
                    else
                        VA.customFailure "not long enough"
                )
            )
        |> VA.requiredAcc fields "w" (VA.stringField VA.validF)
        -- |> VA.optionalAcc fields "r" (\s -> VA.valid (Just s)) Nothing
        |> VA.optionalMaybeAcc fields
            "r"
            (\s ->
                if s == "dd" then
                    VA.validF s
                else
                    VA.customFailure "not good"
            )
        |> VA.fieldGroupAcc fields "group1" juleValidate


juleValidate : VA.Validate String String Jule
juleValidate fields =
    VA.valid Jule
        |> VA.requiredAcc fields "zz" (VA.stringField VA.validF)
        |> VA.requiredAcc fields "test" (VA.stringField VA.validF)
