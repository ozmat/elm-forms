module Tests.Forms.Validation exposing (..)

import Expect
import Test exposing (..)
import Fuzz exposing (bool, string)
import Validation as VA
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
            [ test "helps validating a two fields with String/Password matching values" <|
                \_ ->
                    passwordMatch success (V.string "notempty") (V.string "notempty")
                        |> Expect.equal (success "notempty")
            , test "fails if not String Values" <|
                \_ ->
                    passwordMatch success (V.string "") (V.bool True)
                        |> Expect.equal (failure WrongType)
            , test "first empty string" <|
                \_ ->
                    passwordMatch success (V.string "") (V.string "")
                        |> Expect.equal (failure EmptyString)
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
        , test "Validation.toTuple turns a FormError into a Tuple" <|
            \_ ->
                toTuple (FormError "key" WrongType)
                    |> Expect.equal ( "key", WrongType )
        , describe "Validation.toResult"
            [ fuzz string " turns a FormValidation into a Result (Success)" <|
                \s ->
                    toResult (valid s)
                        |> Expect.equal (Ok s)
            , fuzz string " turns a FormValidation into a Result (Failure)" <|
                \s ->
                    toResult (VA.failure (FormError s WrongType))
                        |> Expect.equal (Err [ ( s, WrongType ) ])
            ]
        , describe "Validation.required"
            [ fuzz string "helps validating a required Field" <|
                \s ->
                    required (fields1 s) "key1" (stringValid success) (valid Required)
                        |> Expect.equal (valid (Required s))
            , test "fails if the field is missing" <|
                \_ ->
                    required (fields1 "") "notfound" (stringValid success) (valid Required)
                        |> Expect.equal (formE "notfound" MissingField)
            , test "fails if the validation fails" <|
                \_ ->
                    required (fields1 "notaa") "key1" (stringValid valFail) (valid Required)
                        |> Expect.equal (formE "key1" (CustomError "error"))
            ]
        , fuzz string "Validation.hardcoded helps harcoding a value during a Validation" <|
            \s ->
                hardcoded s (valid Required)
                    |> Expect.equal (valid (Required s))
        , describe "Validation.optional"
            [ test "helps validating an optional Field with a default value" <|
                \_ ->
                    optional (fields1 "notempty") "key1" success "" (valid Required)
                        |> Expect.equal (valid (Required "notempty"))
            , fuzz string "fallback on default value if empty" <|
                \s ->
                    optional (fields1 "") "key1" success s (valid Required)
                        |> Expect.equal (valid (Required s))
            , test "fails if the field is missing" <|
                \_ ->
                    optional (fields1 "") "notfound" success "" (valid Required)
                        |> Expect.equal (formE "notfound" MissingField)
            , test "fails if the validation fails" <|
                \_ ->
                    optional (fields1 "notaa") "key1" valFail "" (valid Required)
                        |> Expect.equal (formE "key1" (CustomError "error"))
            ]
        , describe "Validation.optionalMaybe"
            [ test "helps validating an optional Field with a Maybe" <|
                \_ ->
                    optionalMaybe (fields1 "notempty") "key1" success (valid OptionalMaybe)
                        |> Expect.equal (valid (OptionalMaybe (Just "notempty")))
            , fuzz string "fallback with Maybe if empty" <|
                \s ->
                    optionalMaybe (fields1 "") "key1" success (valid OptionalMaybe)
                        |> Expect.equal (valid (OptionalMaybe Nothing))
            , test "fails if the field is missing" <|
                \_ ->
                    optionalMaybe (fields1 "") "notfound" success (valid OptionalMaybe)
                        |> Expect.equal (formE "notfound" MissingField)
            , test "fails if the validation fails" <|
                \_ ->
                    optionalMaybe (fields1 "notaa") "key1" valFail (valid OptionalMaybe)
                        |> Expect.equal (formE "key1" (CustomError "error"))
            ]
        , describe "Validation.twoFields"
            [ test "helps validating two fields together" <|
                \_ ->
                    twoFields (fields2 "notempty" "notempty") "key2" "key3" (passwordMatch success) (valid Required)
                        |> Expect.equal (valid (Required "notempty"))
            , test "fails if the first field is missing" <|
                \_ ->
                    twoFields (fields2 "" "") "notfound1" "key3" (passwordMatch success) (valid Required)
                        |> Expect.equal (formE "notfound1" MissingField)
            , test "fails if the second field is missing" <|
                \_ ->
                    twoFields (fields2 "" "") "key2" "notfound2" (passwordMatch success) (valid Required)
                        |> Expect.equal (formE "notfound2" MissingField)
            , test "fails if both fields are missing" <|
                \_ ->
                    twoFields (fields2 "" "") "notfound1" "notfound2" (passwordMatch success) (valid Required)
                        |> Expect.equal (formEL "notfound1" "notfound2" MissingField)
            , test "fails on both fields if the validation fails" <|
                \_ ->
                    twoFields (fields2 "a" "b") "key2" "key3" (passwordMatch success) (valid Required)
                        |> Expect.equal (formEL "key2" "key3" (PasswordNotEqual))
            ]
        , describe "Validation.fieldGroup"
            [ fuzz string "helps validating a FieldGroup" <|
                \s ->
                    fieldGroup (fields3 s) "group1" (\f -> required f "key4" (stringValid success) (valid Required)) (valid FieldGroup)
                        |> Expect.equal (valid (FieldGroup (Required s)))
            , test "fails if the group is missing" <|
                \_ ->
                    fieldGroup (fields3 "") "notfound" (\_ -> valid (Required "")) (valid FieldGroup)
                        |> Expect.equal (formE "notfound" MissingField)
            , test "fails if the group validation fails" <|
                \_ ->
                    fieldGroup (fields3 "notaa") "group1" (\f -> required f "key4" (stringValid valFail) (valid Required)) (valid FieldGroup)
                        |> Expect.equal (formE "key4" (CustomError "error"))
            ]
        ]



-- Fixutres


fields1 : String -> F.Group String
fields1 s =
    F.fields
        [ ( "key1", F.stringWithValue s ) ]


fields2 : String -> String -> F.Group String
fields2 s1 s2 =
    F.fields
        [ ( "key2", F.stringWithValue s1 )
        , ( "key3", F.stringWithValue s2 )
        ]


fields3 : String -> F.Group String
fields3 s =
    F.fields
        [ ( "group1"
          , F.group
                [ ( "key4", F.stringWithValue s ) ]
          )
        ]


type alias Required =
    { a : String
    }


type alias OptionalMaybe =
    { a : Maybe String
    }


type alias FieldGroup =
    { a : Required
    }



-- Helpers


formE_ : comparable -> FieldError err -> FormError comparable err
formE_ comparable fe =
    FormError comparable fe


formE : comparable -> FieldError err -> FormValidation comparable err a
formE comparable fe =
    VA.failure (formE_ comparable fe)


formEL : comparable -> comparable -> FieldError err -> FormValidation comparable err a
formEL comparable1 comparable2 fe =
    VA.Failure (VA.ErrorList [ formE_ comparable1 fe, formE_ comparable2 fe ])


valFail : String -> FieldValidation String String
valFail =
    validation "error" ((==) "aa")
