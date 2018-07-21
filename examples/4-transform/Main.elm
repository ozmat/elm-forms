module Main exposing (..)

import Debug exposing (log)
import Dict exposing (Dict)
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
    ( Model (F.form myFormFields myFormValidate)
    , Cmd.none
    )


type alias Model =
    { myForm : F.Form String MyFormError OtherModel
    }



{- Form -}


type alias OtherModel =
    { -- Notice here that some types of the form result records are different
      -- than the form field ones
      isRed : Bool
    , floatNumber : Float
    , repeated : Dict String Int
    }


type MyFormError
    = NotInt


myFormFields : FF.Fields String
myFormFields =
    -- We can also see here that we have more form fields than
    -- form result records. And that's because we're going to tranform those
    -- field values into the form result record types
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


myFormValidate : FV.Validate String MyFormError OtherModel
myFormValidate fields =
    -- The validate function doesn't stop you from changing the types of the
    -- field values because its goal is only to validate an `OtherModel` (here)
    FV.valid OtherModel
        -- So here we are turning the string into a `Bool` by testing if it's
        -- equal to "red"
        |> FV.required fields "red" (FV.stringField <| FV.success << (==) "red")
        -- This one is a bit silly because we're asking for a string
        -- that can be cast into an `Int` in order to multiply/convert it into
        -- a `Float`. But the point is to show you that you can play with types
        |> FV.required fields "float" (FV.stringField <| FV.int NotInt <| FV.success << (*) 5.4 << toFloat)
        -- Finally we want to count all occurences of the "repeat" fields and
        -- the result needs to be a `Dict String Int`. So we are using a
        -- `fieldgroup` with a specific validate function to achieve this.
        |> FV.fieldgroup fields "repeat_group" repeatValidate


repeatValidate : FV.Validate String MyFormError (Dict String Int)
repeatValidate fields =
    -- Notice that the `Validate` result type is `Dict String Int`.
    -- This validate function doesn't validate a model/type alias like
    -- we previously did. It actually takes a function : `repeatCount`. But the
    -- model/type alias we were using are also functions (the `OtherModel`
    -- function is `Bool -> Float -> Dict String Int -> OtherModel`). So as
    -- long as you respect the order/types in the `Validate` function you can
    -- basically use any function to create the result
    let
        lowerThemAll =
            -- Here we just make sure that all strings are lowered
            FV.stringField <| FV.success << String.toLower
    in
    -- And `repeatCount` will do the job
    FV.valid repeatCount
        |> FV.required fields "repeat1" lowerThemAll
        |> FV.required fields "repeat2" lowerThemAll
        |> FV.required fields "repeat3" lowerThemAll
        |> FV.required fields "repeat4" lowerThemAll
        |> FV.required fields "repeat5" lowerThemAll


repeatCount : String -> String -> String -> String -> String -> Dict String Int
repeatCount s1 s2 s3 s4 s5 =
    -- This function takes 5 strings (we have 5 "repeat" fields) and count
    -- all the occurences
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
        , onInput (FU.stringFieldMsg Form fieldName)
        ]
        []
