module Tests.Forms.Validation exposing (..)

import Expect
import Test exposing (..)
import Fuzz exposing (bool, string)
import Vi.Validation as VA
import Forms.Validation exposing (..)
import Forms.Value as V
import Forms.Field as F


all : Test
all =
    describe "Forms.Validation tests"
        [ test "Validation.failure creates a failed FieldValidation" <|
            \_ ->
                failure MissingField
                    |> Expect.equal (VA.failure MissingField)
        , fuzz string "Validation.customFailure creates a failed FieldValidation with a CustomError" <|
            \s ->
                customFailure s
                    |> Expect.equal (VA.failure (CustomError s))
        , fuzz string "Validation.success creates a successful FieldValidation" <|
            \s ->
                success s
                    |> Expect.equal (VA.success s)
        , fuzz string "Validation.validation helps creating a basic FieldValidation function" <|
            \s ->
                validation s String.isEmpty "notempty"
                    |> Expect.equal (customFailure s)
        , describe "Validation.stringValid"
            [ fuzz string "helps validating a field with a String value" <|
                \s ->
                    stringValid success (V.string s)
                        |> Expect.equal (success s)
            , test "fails otherwise" <|
                \_ ->
                    stringValid success (V.bool True)
                        |> Expect.equal (failure WrongType)
            ]
        , describe "Validation.boolValid"
            [ fuzz bool "helps validating a field with a Bool value" <|
                \b ->
                    boolValid success (V.bool b)
                        |> Expect.equal (success b)
            , test "fails otherwise" <|
                \_ ->
                    boolValid success (V.string "valid")
                        |> Expect.equal (failure WrongType)
            ]
        , describe "Validation.int"
            [ test "helps validating a field with a String/Int value" <|
                \_ ->
                    stringValid (int success) (V.string "100")
                        |> Expect.equal (success 100)
            , test "fails otherwise" <|
                \_ ->
                    stringValid (int success) (V.string "notint")
                        |> Expect.equal (failure NotInt)
            ]
        , describe "Validation.float"
            [ test "helps validating a field with a String/Float value" <|
                \_ ->
                    stringValid (float success) (V.string "5.54")
                        |> Expect.equal (success 5.54)
            , test "fails otherwise" <|
                \_ ->
                    stringValid (float success) (V.string "notfloat")
                        |> Expect.equal (failure NotFloat)
            ]
        , describe "Validation.notEmpty"
            [ test "helps validating a field with a non empty String value" <|
                \_ ->
                    stringValid (notEmpty success) (V.string "notempty")
                        |> Expect.equal (success "notempty")
            , test "fails otherwise" <|
                \_ ->
                    stringValid (notEmpty success) (V.string "")
                        |> Expect.equal (failure EmptyString)
            ]
        , describe "Validation.length"
            [ test "helps validating a field with a String value of a specific length" <|
                \_ ->
                    stringValid (length 2 5 success) (V.string "abc")
                        |> Expect.equal (success "abc")
            , test "fails otherwise" <|
                \_ ->
                    stringValid (length 2 5 success) (V.string "ab")
                        |> Expect.equal (failure WrongLength)
            ]
        , describe "Validation.email"
            [ test "helps validating a field with a String/email value" <|
                \_ ->
                    stringValid (email success) (V.string "abc@abc.com")
                        |> Expect.equal (success "abc@abc.com")
            , test "fails otherwise" <|
                \_ ->
                    stringValid (email success) (V.string "notemail")
                        |> Expect.equal (failure NotEmail)
            ]
        , describe "Validation.passwordMatch"
            [ fuzz string "helps validating a two fields with String/Password matching values" <|
                \s ->
                    passwordMatch success (V.string s) (V.string s)
                        |> Expect.equal (success s)
            , test "fails if not String Values" <|
                \_ ->
                    passwordMatch success (V.string "") (V.bool True)
                        |> Expect.equal (failure WrongType)
            , test "fails if not equal" <|
                \_ ->
                    passwordMatch success (V.string "ab") (V.string "abc")
                        |> Expect.equal (failure PasswordNotEqual)
            ]
        , test "Validation.FieldValidation can return any types" <|
            \_ ->
                stringValid (\_ -> success [ 1, 2 ]) (V.string "whatever")
                    |> Expect.equal (VA.success [ 1, 2 ])
        , fuzz string "Validation.valid creates a successful FormdValidation" <|
            \s ->
                valid s
                    |> Expect.equal (VA.success s)
        , fuzz string "Validation.toTuple turns a FormError into a Tuple" <|
            \s ->
                toTuple (FormError "key" WrongType)
                    |> Expect.equal ( "key", WrongType )
        ]
