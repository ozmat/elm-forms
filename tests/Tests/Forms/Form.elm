module Tests.Forms.Form exposing (..)

import Dict as D
import Expect
import Test exposing (..)
import Fuzz exposing (string, bool)
import Forms.Form exposing (..)
import Forms.Field as F
import Forms.Validation as V
import Tests.Forms.Validation as TV


all : Test
all =
    describe "Forms.Form tests"
        [ test "Form.form creates a Form" <|
            \_ ->
                form1 "" validate1
                    |> Expect.equal (Form (TV.fields1 "") validate1)
        , describe "Form.validate"
            [ fuzz string "helps validating a form" <|
                \s ->
                    validate (form1 s validate1)
                        |> Expect.equal (V.Valid (TV.Required s))
            , test "it may be invalid" <|
                \_ ->
                    validate (form1 "notaa" (validate2 "key1"))
                        |> Expect.equal (V.Invalid (D.fromList [ ( "key1", "error" ) ]))
            , test "or have errors" <|
                \_ ->
                    validate (form1 "notaa" (validate2 "notfound"))
                        |> Expect.equal (V.Error (D.fromList [ ( "notfound", V.MissingField ) ]))
            ]
        , describe "Form.validateWithFieldErrors"
            [ fuzz string "helps validating a form" <|
                \s ->
                    validateWithFieldErrors (form1 s validate1)
                        |> Expect.equal (Ok (TV.Required s))
            , test "returns a dict of FieldError otherwise" <|
                \_ ->
                    validateWithFieldErrors (form1 "notaa" (validate2 "key1"))
                        |> Expect.equal (Err (D.fromList [ ( "key1", V.CustomErr "error" ) ]))
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


form1 : String -> V.Validate String String TV.Required -> Form String String TV.Required
form1 s =
    form (TV.fields1 s)


validate1 : V.Validate String String TV.Required
validate1 fields =
    V.required fields "key1" (V.stringField V.success) (V.valid TV.Required)


validate2 : String -> V.Validate String String TV.Required
validate2 key fields =
    V.required fields key (V.stringField TV.valFail) (V.valid TV.Required)


fields2 : Bool -> F.Fields String
fields2 b =
    F.fields
        [ ( "key2", F.checkboxWithDefault b ) ]
