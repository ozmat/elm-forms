module Tests.Forms.Form exposing (..)

import Dict as D
import Expect
import Test exposing (..)
import Fuzz exposing (string)
import Forms.Form exposing (..)
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
                    validate (form1 "notaa" validate2)
                        |> Expect.equal (V.Invalid (D.fromList [ ( "key1", "error" ) ]))
            , test "or have errors" <|
                \_ ->
                    validate (form1 "notaa" validate3)
                        |> Expect.equal (V.Error (D.fromList [ ( "notfound", V.MissingField ) ]))
            ]
        , describe "Form.validateWithFieldErrors"
            [ fuzz string "helps validating a form" <|
                \s ->
                    validateWithFieldErrors (form1 s validate1)
                        |> Expect.equal (Ok (TV.Required s))
            , test "returns a dict of FieldError otherwise" <|
                \_ ->
                    validateWithFieldErrors (form1 "notaa" validate2)
                        |> Expect.equal (Err (D.fromList [ ( "key1", V.CustomErr "error" ) ]))
            ]
        ]



-- Fixtures


form1 : String -> V.Validate String String TV.Required -> Form String String TV.Required
form1 s =
    form (TV.fields1 s)


validate1 : V.Validate String String TV.Required
validate1 fields =
    V.required fields "key1" (V.stringField V.success) (V.valid TV.Required)


validate2 : V.Validate String String TV.Required
validate2 fields =
    V.required fields "key1" (V.stringField TV.valFail) (V.valid TV.Required)


validate3 : V.Validate String String TV.Required
validate3 fields =
    V.required fields "notfound" (V.stringField TV.valFail) (V.valid TV.Required)
