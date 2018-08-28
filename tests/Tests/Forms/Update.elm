module Tests.Forms.Update exposing (all)

import Expect
import Forms.Field as FF
import Forms.Field.Internal as IF
import Forms.Form.Internal as IFO
import Forms.Update as U exposing (..)
import Forms.Update.Internal exposing (Msg(..))
import Forms.Validation as FV
import Forms.Value as V
import Fuzz as F
import Random
import Test exposing (..)


all : Test
all =
    describe "Forms.Update tests"
        [ fuzz F.string "creates a wrapped UpdateStringField msg" <|
            \s ->
                stringFieldMsg Form "field" s
                    |> Expect.equal (Form <| UpdateStringField "field" s)
        , fuzz F.bool "creates a wrapped UpdateBoolField msg" <|
            \b ->
                boolFieldMsg Form "field" b
                    |> Expect.equal (Form <| UpdateBoolField "field" b)
        , describe "Update.updateForm"
            [ fuzz F.string "updates a string Field when the msg is UpdateStringField" <|
                \s ->
                    testUpdateForm (UpdateStringField "input" s) "input" form
                        |> Expect.equal (Just (V.string s))
            , fuzz F.bool "updates a bool Field when the msg is UpdateBoolField" <|
                \b ->
                    testUpdateForm (UpdateBoolField "checkbox" b) "checkbox" form
                        |> Expect.equal (Just (V.bool b))
            , test "does nothing if the field does not exist" <|
                \_ ->
                    updateForm (UpdateStringField "notfound" "value") form
                        |> Expect.equal form
            ]
        , describe "Update.stringFieldCommands"
            [ test "add commands on string field events" <|
                \_ ->
                    myUpdate 0 (Form <| UpdateStringField "input" "") init
                        |> Expect.equal ( init, testCmd )
            , test "has no impact on the wrong field" <|
                \_ ->
                    myUpdate 0 (Form <| UpdateStringField "wrong" "") init
                        |> Expect.equal ( init, Cmd.none )
            , test "has no impact on the wrong message" <|
                \_ ->
                    myUpdate 0 (Form <| UpdateBoolField "checkbox" False) init
                        |> Expect.equal ( init, Cmd.none )
            ]
        , describe "Update.boolFieldCommands"
            [ test "add commands on bool field events" <|
                \_ ->
                    myUpdate 1 (Form <| UpdateBoolField "checkbox" False) init
                        |> Expect.equal ( init, testCmd )
            , test "has no impact on the wrong field" <|
                \_ ->
                    myUpdate 1 (Form <| UpdateBoolField "wrong" False) init
                        |> Expect.equal ( init, Cmd.none )
            , test "has no impact on the wrong message" <|
                \_ ->
                    myUpdate 1 (Form <| UpdateStringField "input" "") init
                        |> Expect.equal ( init, Cmd.none )
            ]
        , describe "Update.formCommands"
            [ test "add commands on string field events" <|
                \_ ->
                    myUpdate 2 (Form <| UpdateStringField "input" "") init
                        |> Expect.equal ( init, testCmd )
            , test "add commands on bool field events" <|
                \_ ->
                    myUpdate 2 (Form <| UpdateBoolField "checkbox" False) init
                        |> Expect.equal ( init, testCmd )
            , test "has no impact on the wrong string field" <|
                \_ ->
                    myUpdate 2 (Form <| UpdateStringField "wrong" "") init
                        |> Expect.equal ( init, Cmd.none )
            , test "has no impact on the wrong bool field" <|
                \_ ->
                    myUpdate 2 (Form <| UpdateBoolField "wrong" False) init
                        |> Expect.equal ( init, Cmd.none )
            ]
        ]



-- Update form


fields : FF.Fields String
fields =
    FF.fields
        [ ( "input", FF.input )
        , ( "checkbox", FF.checkbox )
        ]


validate : FV.Validate String () String
validate _ =
    FV.valid "whatever"


form : IFO.Form String () String
form =
    IFO.Form fields validate


testUpdateForm : U.Msg comparable -> comparable -> IFO.Form comparable err a -> Maybe V.Value
testUpdateForm msg key f =
    let
        (IFO.Form newFields _) =
            updateForm msg f
    in
    IF.getValue key newFields



-- Effects


type alias Model =
    { aform : IFO.Form String () String
    }


init : Model
init =
    Model form


type MyMsg
    = Form (U.Msg String)
    | TestEffects Int


testCmd : Cmd MyMsg
testCmd =
    Random.generate TestEffects (Random.int 1 150)


mySEffects : Model -> String -> String -> ( Model, Cmd MyMsg )
mySEffects model key value =
    case key of
        "input" ->
            ( model, testCmd )

        _ ->
            ( model, Cmd.none )


myBEffects : Model -> String -> Bool -> ( Model, Cmd MyMsg )
myBEffects model key value =
    case key of
        "checkbox" ->
            ( model, testCmd )

        _ ->
            ( model, Cmd.none )


myUpdate : Int -> MyMsg -> Model -> ( Model, Cmd MyMsg )
myUpdate i msg model =
    let
        cmd mo m =
            case i of
                0 ->
                    stringFieldCommands mo m mySEffects

                1 ->
                    boolFieldCommands mo m myBEffects

                2 ->
                    formCommands mo m mySEffects myBEffects

                _ ->
                    ( mo, Cmd.none )
    in
    case msg of
        Form formMsg ->
            let
                newModel =
                    { model | aform = updateForm formMsg model.aform }
            in
            cmd newModel formMsg

        TestEffects _ ->
            ( model, Cmd.none )
