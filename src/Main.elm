module Main exposing (..)

import Html exposing (Html, Attribute, beginnerProgram, text, div, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Debug exposing (log)


-- Can't reexport ...

import Ki.Value as V exposing (..)
import Ki.Field as F exposing (..)
import Ki.Validation as VA exposing (..)
import Ki.Form as FO
import Ki.Update as U


-- MAIN


main : Program Never Model Msg
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
                    log "" (FO.validateD newModel.form)
            in
                newModel



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ inputText ">2" "z"
        , inputText "optional maybe == dd" "r"
        , inputText "== qq" "zz"
        , inputText ">0 && <6" "test"
        , inputText "notEmpty" "der"
        , inputText "should match" "password"
        , inputText "should match" "passwordA"
        , inputText "optional int" "int"
        , inputText "optional float" "float"
        , inputText "is mail" "mail"

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
        , ( "r", F.string )
        , ( "group1"
          , F.group
                [ ( "zz", F.string )
                , ( "test", F.string )
                , ( "der", F.string )
                ]
          )
        , ( "password", F.string )
        , ( "passwordA", F.string )
        , ( "int", F.string )
        , ( "float", F.string )
        , ( "mail", F.string )
        ]


type alias Jean =
    { a : String
    , c : Maybe String
    , d : Jule
    , password : String
    , i : Int
    , f : Float
    , m : String
    }


type alias Jule =
    { a : String
    , b : String
    , c : String
    }


test3 : VA.Validate String String Jean
test3 fields =
    VA.valid Jean
        |> VA.required fields
            "z"
            (VA.stringValid
                (\s ->
                    if String.length s > 2 then
                        VA.success s
                    else
                        VA.customFailure "not long enough"
                )
            )
        |> VA.optionalMaybe fields
            "r"
            (\s ->
                if s == "dd" then
                    VA.success s
                else
                    VA.customFailure "not good"
            )
        |> VA.fieldGroup fields "group1" juleValidate
        |> VA.twoFields fields "password" "passwordA" (VA.passwordMatch VA.success)
        |> VA.optional fields "int" (VA.int VA.success) 0
        |> VA.optional fields "float" (VA.float VA.success) 15.5
        |> VA.required fields "mail" (VA.stringValid <| VA.email VA.success)


juleValidate : VA.Validate String String Jule
juleValidate fields =
    VA.valid Jule
        |> VA.required fields "zz" (VA.stringValid <| VA.validation "not good2" ((==) "qq"))
        |> VA.required fields "test" (VA.stringValid <| VA.length 0 6 VA.success)
        |> VA.required fields "der" (VA.stringValid <| VA.notEmpty VA.success)
