module Tests.Forms.Form exposing (all)

import Dict as D
import Expect
import Forms.Field as F
import Forms.Form exposing (..)
import Forms.Form.Internal as IFO
import Forms.Validation as V
import Forms.Validation.Result as VR
import Fuzz exposing (bool, string)
import Test exposing (..)


all : Test
all =
    describe "Forms.Form tests"
        [ test "Form.form creates a Form" <|
            \_ ->
                form1 "" validate1
                    |> Expect.equal (IFO.Form (fields1 "") validate1)
        , describe "Form.validate"
            [ fuzz string "helps validating a form" <|
                \s ->
                    validate (form1 s validate1)
                        |> Expect.equal (VR.Valid (Required s))
            , test "it may be invalid" <|
                \_ ->
                    validate (form1 "" (validate2 "key1"))
                        |> Expect.equal (VR.Invalid (D.fromList [ ( "key1", "error" ) ]))
            , test "or have errors" <|
                \_ ->
                    validate (form1 "" (validate2 "notfound"))
                        |> Expect.equal (VR.Error (D.fromList [ ( "notfound", VR.MissingField ) ]))
            ]
        , describe "Forms.getStringField" <|
            [ fuzz string "gets the value of a String Field" <|
                \s ->
                    getStringField "key1" (form1 s validate1)
                        |> Expect.equal (Just s)
            , test "returns Nothing if it's a Bool Field" <|
                \_ ->
                    getStringField "key2" (form (fields2 False) validate1)
                        |> Expect.equal Nothing
            , test "returns Nothing if the Field cannot be found" <|
                \_ ->
                    getStringField "notfound" (form1 "" validate1)
                        |> Expect.equal Nothing
            ]
        , describe "Forms.getBoolField" <|
            [ fuzz bool "gets the value of a Bool Field" <|
                \b ->
                    getBoolField "key2" (form (fields2 b) validate1)
                        |> Expect.equal (Just b)
            , test "returns Nothing if it's a String Field" <|
                \_ ->
                    getBoolField "key1" (form1 "" validate1)
                        |> Expect.equal Nothing
            , test "returns Nothing if the Field cannot be found" <|
                \_ ->
                    getBoolField "notfound" (form1 "" validate1)
                        |> Expect.equal Nothing
            ]
        , describe "Forms.setStringField" <|
            [ fuzz string "sets the value of a String Field" <|
                \s ->
                    setStringField "key1" s (form1 "" validate1)
                        |> Expect.equal (form1 s validate1)
            , test "does nothing if it's a Bool Field" <|
                \_ ->
                    setStringField "key2" "dd" (form (fields2 False) validate1)
                        |> Expect.equal (form (fields2 False) validate1)
            , test "does nothing if the Field cannot be found" <|
                \_ ->
                    setStringField "notfound" "dd" (form1 "" validate1)
                        |> Expect.equal (form1 "" validate1)
            ]
        , describe "Forms.setBoolField" <|
            [ fuzz bool "sets the value of a Bool Field" <|
                \b ->
                    setBoolField "key2" b (form (fields2 False) validate1)
                        |> Expect.equal (form (fields2 b) validate1)
            , test "does nothing if it's a String Field" <|
                \_ ->
                    setBoolField "key1" False (form1 "" validate1)
                        |> Expect.equal (form1 "" validate1)
            , test "does nothing if the Field cannot be found" <|
                \_ ->
                    setBoolField "notfound" False (form1 "" validate1)
                        |> Expect.equal (form1 "" validate1)
            ]
        ]



-- Fixtures


fields1 : String -> F.Fields String
fields1 s =
    F.fields
        [ ( "key1", F.inputWithDefault s ) ]


type alias Required =
    { a : String
    }


form1 : String -> V.Validate String String Required -> Form String String Required
form1 s =
    form (fields1 s)


validate1 : V.Validate String String Required
validate1 fields =
    V.required fields "key1" (V.stringField V.success) (V.valid Required)


validate2 : String -> V.Validate String String Required
validate2 key fields =
    V.required fields key (V.stringField <| \_ -> V.failure "error") (V.valid Required)


fields2 : Bool -> F.Fields String
fields2 b =
    F.fields
        [ ( "key2", F.checkboxWithDefault b ) ]
