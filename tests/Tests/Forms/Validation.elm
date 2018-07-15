module Tests.Forms.Validation exposing (..)

import Dict exposing (Dict)
import Expect
import Forms.Field as F
import Forms.Validation exposing (..)
import Forms.Value as V
import Fuzz exposing (bool, string)
import Test exposing (..)
import Validation as VA


all : Test
all =
    describe "Forms.Validation tests"
        [ test "Validation.configFailure creates a failed FieldValidation with a ConfigError" <|
            \_ ->
                configFailure WrongType
                    |> Expect.equal (VA.failure wrongType)
        , fuzz string "Validation.failure creates a failed FieldValidation with a CustomError" <|
            \s ->
                failure s
                    |> Expect.equal (VA.failure (CustomErr s))
        , fuzz string "Validation.success creates a successful FieldValidation" <|
            \s ->
                success s
                    |> Expect.equal (VA.success s)
        , describe "Validation.validation"
            [ test "helps creating a basic FieldValidation function" <|
                \_ ->
                    validation "error" String.isEmpty ""
                        |> Expect.equal (success "")
            , fuzz string "that may fail" <|
                \s ->
                    validation s String.isEmpty "notempty"
                        |> Expect.equal (failure s)
            ]
        , describe "Validation.stringField"
            [ fuzz string "helps validating a string field (input/select)" <|
                \s ->
                    stringField success (V.string s)
                        |> Expect.equal (success s)
            , test "fails otherwise" <|
                \_ ->
                    stringField success (V.bool True)
                        |> Expect.equal (configFailure WrongType)
            ]
        , describe "Validation.boolField"
            [ fuzz bool "helps validating a bool field (checkbox)" <|
                \b ->
                    boolField success (V.bool b)
                        |> Expect.equal (success b)
            , test "fails otherwise" <|
                \_ ->
                    boolField success (V.string "valid")
                        |> Expect.equal (configFailure WrongType)
            ]
        , describe "Validation.isChecked"
            [ test "helps validating a field with a Bool/True value" <|
                \_ ->
                    boolField (isChecked NotChecked success) (V.bool True)
                        |> Expect.equal (success True)
            , test "fails otherwise" <|
                \_ ->
                    boolField (isChecked NotChecked success) (V.bool False)
                        |> Expect.equal (failure NotChecked)
            ]
        , describe "Validation.int"
            [ test "helps validating a field with a String/Int value" <|
                \_ ->
                    stringField (int NotInt success) (V.string "100")
                        |> Expect.equal (success 100)
            , test "fails otherwise" <|
                \_ ->
                    stringField (int NotInt success) (V.string "notint")
                        |> Expect.equal (failure NotInt)
            ]
        , describe "Validation.float"
            [ test "helps validating a field with a String/Float value" <|
                \_ ->
                    stringField (float NotFloat success) (V.string "5.54")
                        |> Expect.equal (success 5.54)
            , test "fails otherwise" <|
                \_ ->
                    stringField (float NotFloat success) (V.string "notfloat")
                        |> Expect.equal (failure NotFloat)
            ]
        , describe "Validation.notEmpty"
            [ test "helps validating a field with a non empty String value" <|
                \_ ->
                    stringField (notEmpty EmptyString success) (V.string "notempty")
                        |> Expect.equal (success "notempty")
            , test "fails otherwise" <|
                \_ ->
                    stringField (notEmpty EmptyString success) (V.string "")
                        |> Expect.equal (failure EmptyString)
            ]
        , describe "Validation.length"
            [ test "helps validating a field with a String value of a specific length" <|
                \_ ->
                    stringField (length 2 5 WrongLength success) (V.string "abc")
                        |> Expect.equal (success "abc")
            , test "fails otherwise" <|
                \_ ->
                    stringField (length 2 5 WrongLength success) (V.string "ab")
                        |> Expect.equal (failure WrongLength)
            ]
        , describe "Validation.email"
            [ test "helps validating a field with a String/email value" <|
                \_ ->
                    stringField (email NotEmail success) (V.string "abc@abc.com")
                        |> Expect.equal (success "abc@abc.com")
            , test "fails otherwise" <|
                \_ ->
                    stringField (email NotEmail success) (V.string "notemail")
                        |> Expect.equal (failure NotEmail)
            ]
        , describe "Validation.passwordMatch"
            [ test "helps validating two fields with String/Password matching values" <|
                \_ ->
                    passwordMatch PasswordNotEqual success (V.string "notempty") (V.string "notempty")
                        |> Expect.equal (success "notempty")
            , test "fails if not String Values" <|
                \_ ->
                    passwordMatch PasswordNotEqual success (V.string "") (V.bool True)
                        |> Expect.equal (configFailure WrongType)
            , test "fails if not equal" <|
                \_ ->
                    passwordMatch PasswordNotEqual success (V.string "ab") (V.string "abc")
                        |> Expect.equal (failure PasswordNotEqual)
            ]
        , test "Validation.FieldValidation can return any types" <|
            \_ ->
                stringField (\_ -> success [ 1, 2 ]) (V.string "whatever")
                    |> Expect.equal (VA.success [ 1, 2 ])
        , fuzz string "Validation.valid creates a successful FormdValidation" <|
            \s ->
                valid s
                    |> Expect.equal (VA.success s)
        , test "Validation.toTuple turns a FormError into a Tuple" <|
            \_ ->
                toTuple (FormError "key" wrongType)
                    |> Expect.equal ( "key", wrongType )
        , describe "Validation.toResult"
            [ fuzz string " turns a FormValidation into a Result (Success)" <|
                \s ->
                    toResult (valid s)
                        |> Expect.equal (Ok s)
            , fuzz string " turns a FormValidation into a Result (Failure)" <|
                \s ->
                    toResult (VA.failure (FormError s wrongType))
                        |> Expect.equal (Err [ ( s, wrongType ) ])
            ]
        , describe "Validation.filterErrors"
            [ fuzz string "returns CustomErrors if there are no ConfigErrors" <|
                \s ->
                    filterErrors
                        [ FormError "a" (CustomErr s)
                        , FormError "b" (CustomErr s)
                        ]
                        |> Expect.equal (Ok [ ( "b", s ), ( "a", s ) ])
            , test "returns ConfigErrors otherwise (2)" <|
                \_ ->
                    filterErrors
                        [ FormError "a" wrongType
                        , FormError "b" missingField
                        , FormError "c" (CustomErr "whatever")
                        ]
                        |> Expect.equal (Err [ ( "b", MissingField ), ( "a", WrongType ) ])
            , test "returns ConfigErrors otherwise (1)" <|
                \_ ->
                    filterErrors
                        [ FormError "a" wrongType
                        , FormError "b" (CustomErr "whatever1")
                        , FormError "c" (CustomErr "whatever2")
                        ]
                        |> Expect.equal (Err [ ( "a", WrongType ) ])
            ]
        , describe "Validation.toFormResult"
            [ fuzz string "converts a FormValidation into a FormResult" <|
                \s ->
                    toFormResult (validate1 (fieldsValidate1 s))
                        |> Expect.equal (Valid (SomeResult s s s))
            , test "it may have ConfigErrors (1)" <|
                \_ ->
                    toFormResult (validate2 (fieldsValidate1 ""))
                        |> Expect.equal (Error (Dict.fromList [ ( "notfound", MissingField ) ]))
            , test "it may have ConfigErrors (2)" <|
                \_ ->
                    toFormResult (validate2 fieldsValidate2)
                        |> Expect.equal (Error (Dict.fromList [ ( "b", WrongType ), ( "notfound", MissingField ) ]))
            , fuzz string "it may have CustomErrors (1)" <|
                \s ->
                    toFormResult (validate3 s (fieldsValidate1 ""))
                        |> Expect.equal (Invalid (Dict.fromList [ ( "a", s ) ]))
            , fuzz string "it may have CustomErrors (2)" <|
                \s ->
                    toFormResult (validate4 s (fieldsValidate1 ""))
                        |> Expect.equal (Invalid (Dict.fromList [ ( "b", s ), ( "a", s ) ]))
            ]
        , describe "Validation.required"
            [ fuzz string "helps validating a required Field" <|
                \s ->
                    required (fields1 s) "key1" (stringField success) (valid Required)
                        |> Expect.equal (valid (Required s))
            , test "fails if the field is missing" <|
                \_ ->
                    required (fields1 "") "notfound" (stringField success) (valid Required)
                        |> Expect.equal (formE "notfound" missingField)
            , test "fails if the validation fails" <|
                \_ ->
                    required (fields1 "notaa") "key1" (stringField valFail) (valid Required)
                        |> Expect.equal (formE "key1" (CustomErr "error"))
            ]
        , fuzz string "Validation.hardcoded helps harcoding a value during a Validation" <|
            \s ->
                hardcoded s (valid Required)
                    |> Expect.equal (valid (Required s))
        , describe "Validation.optional"
            [ test "helps validating an optional Field with a default value" <|
                \_ ->
                    optional (fields1 "notempty") "key1" "" success (valid Required)
                        |> Expect.equal (valid (Required "notempty"))
            , fuzz string "fallback on default value if empty" <|
                \s ->
                    optional (fields1 "") "key1" s success (valid Required)
                        |> Expect.equal (valid (Required s))
            , test "fails if the field is missing" <|
                \_ ->
                    optional (fields1 "") "notfound" "" success (valid Required)
                        |> Expect.equal (formE "notfound" missingField)
            , test "fails if the validation fails" <|
                \_ ->
                    optional (fields1 "notaa") "key1" "" valFail (valid Required)
                        |> Expect.equal (formE "key1" (CustomErr "error"))
            ]
        , describe "Validation.optionalWithMaybe"
            [ test "helps validating an optional Field with a Maybe" <|
                \_ ->
                    optionalWithMaybe (fields1 "notempty") "key1" success (valid OptionalMaybe)
                        |> Expect.equal (valid (OptionalMaybe (Just "notempty")))
            , fuzz string "fallback with Maybe if empty" <|
                \s ->
                    optionalWithMaybe (fields1 "") "key1" success (valid OptionalMaybe)
                        |> Expect.equal (valid (OptionalMaybe Nothing))
            , test "fails if the field is missing" <|
                \_ ->
                    optionalWithMaybe (fields1 "") "notfound" success (valid OptionalMaybe)
                        |> Expect.equal (formE "notfound" missingField)
            , test "fails if the validation fails" <|
                \_ ->
                    optionalWithMaybe (fields1 "notaa") "key1" valFail (valid OptionalMaybe)
                        |> Expect.equal (formE "key1" (CustomErr "error"))
            ]
        , describe "Validation.discardable"
            [ test "helps validating a discardable Field" <|
                \_ ->
                    discardable (fields1 "") "key1" success (valid Required)
                        |> Expect.equal (valid Required)
            , test "fails if the field is missing" <|
                \_ ->
                    discardable (fields1 "") "notfound" success (valid Required)
                        |> Expect.equal (formE "notfound" missingField)
            , test "fails if the validation fails" <|
                \_ ->
                    discardable (fields1 "notaa") "key1" (stringField valFail) (valid Required)
                        |> Expect.equal (formE "key1" (CustomErr "error"))
            ]
        , describe "Validation.twoFields"
            [ test "helps validating two fields together" <|
                \_ ->
                    twoFields (fields2 "notempty" "notempty") "key2" "key3" (passwordMatch PasswordNotEqual success) (valid Required)
                        |> Expect.equal (valid (Required "notempty"))
            , test "fails if the first field is missing" <|
                \_ ->
                    twoFields (fields2 "" "") "notfound1" "key3" (passwordMatch PasswordNotEqual success) (valid Required)
                        |> Expect.equal (formE "notfound1" missingField)
            , test "fails if the second field is missing" <|
                \_ ->
                    twoFields (fields2 "" "") "key2" "notfound2" (passwordMatch PasswordNotEqual success) (valid Required)
                        |> Expect.equal (formE "notfound2" missingField)
            , test "fails if both fields are missing" <|
                \_ ->
                    twoFields (fields2 "" "") "notfound1" "notfound2" (passwordMatch PasswordNotEqual success) (valid Required)
                        |> Expect.equal (formEL "notfound1" "notfound2" missingField)
            , test "fails on both fields if the validation fails" <|
                \_ ->
                    twoFields (fields2 "a" "b") "key2" "key3" (passwordMatch PasswordNotEqual success) (valid Required)
                        |> Expect.equal (formEL "key2" "key3" (CustomErr PasswordNotEqual))
            ]
        , describe "Validation.fieldgroup"
            [ fuzz string "helps validating a FieldGroup" <|
                \s ->
                    fieldgroup (fields3 s) "group1" (\f -> required f "key4" (stringField success) (valid Required)) (valid FieldGroup)
                        |> Expect.equal (valid (FieldGroup (Required s)))
            , test "fails if the group is missing" <|
                \_ ->
                    fieldgroup (fields3 "") "notfound" (\_ -> valid (Required "")) (valid FieldGroup)
                        |> Expect.equal (formE "notfound" missingField)
            , test "fails if the group validation fails" <|
                \_ ->
                    fieldgroup (fields3 "notaa") "group1" (\f -> required f "key4" (stringField valFail) (valid Required)) (valid FieldGroup)
                        |> Expect.equal (formE "key4" (CustomErr "error"))
            ]
        , describe "Validation.required1"
            [ fuzz string "helps validating a required Field (bind)" <|
                \s ->
                    required1 (fields1 s) "key1" (stringField success) (valid Required)
                        |> Expect.equal (valid (Required s))
            , test "fails if the field is missing" <|
                \_ ->
                    required1 (fields1 "") "notfound" (stringField success) (valid Required)
                        |> Expect.equal (formE "notfound" missingField)
            , test "fails if the validation fails" <|
                \_ ->
                    required1 (fields1 "notaa") "key1" (stringField valFail) (valid Required)
                        |> Expect.equal (formE "key1" (CustomErr "error"))
            ]
        , fuzz string "Validation.hardcoded1 helps harcoding a value during a Validation (bind)" <|
            \s ->
                hardcoded1 s (valid Required)
                    |> Expect.equal (valid (Required s))
        , describe "Validation.optional1"
            [ test "helps validating an optional Field with a default value (bind)" <|
                \_ ->
                    optional1 (fields1 "notempty") "key1" "" success (valid Required)
                        |> Expect.equal (valid (Required "notempty"))
            , fuzz string "fallback on default value if empty" <|
                \s ->
                    optional1 (fields1 "") "key1" s success (valid Required)
                        |> Expect.equal (valid (Required s))
            , test "fails if the field is missing" <|
                \_ ->
                    optional1 (fields1 "") "notfound" "" success (valid Required)
                        |> Expect.equal (formE "notfound" missingField)
            , test "fails if the validation fails" <|
                \_ ->
                    optional1 (fields1 "notaa") "key1" "" valFail (valid Required)
                        |> Expect.equal (formE "key1" (CustomErr "error"))
            ]
        , describe "Validation.optionalWithMaybe1"
            [ test "helps validating an optional Field with a Maybe (bind)" <|
                \_ ->
                    optionalWithMaybe1 (fields1 "notempty") "key1" success (valid OptionalMaybe)
                        |> Expect.equal (valid (OptionalMaybe (Just "notempty")))
            , fuzz string "fallback with Maybe if empty" <|
                \s ->
                    optionalWithMaybe1 (fields1 "") "key1" success (valid OptionalMaybe)
                        |> Expect.equal (valid (OptionalMaybe Nothing))
            , test "fails if the field is missing" <|
                \_ ->
                    optionalWithMaybe1 (fields1 "") "notfound" success (valid OptionalMaybe)
                        |> Expect.equal (formE "notfound" missingField)
            , test "fails if the validation fails" <|
                \_ ->
                    optionalWithMaybe1 (fields1 "notaa") "key1" valFail (valid OptionalMaybe)
                        |> Expect.equal (formE "key1" (CustomErr "error"))
            ]
        , describe "Validation.discardable1"
            [ test "helps validating a discardable Field (bind)" <|
                \_ ->
                    discardable1 (fields1 "") "key1" success (valid Required)
                        |> Expect.equal (valid Required)
            , test "fails if the field is missing" <|
                \_ ->
                    discardable1 (fields1 "") "notfound" success (valid Required)
                        |> Expect.equal (formE "notfound" missingField)
            , test "fails if the validation fails" <|
                \_ ->
                    discardable1 (fields1 "notaa") "key1" (stringField valFail) (valid Required)
                        |> Expect.equal (formE "key1" (CustomErr "error"))
            ]
        , describe "Validation.twoFields1"
            [ test "helps validating two fields together (bind)" <|
                \_ ->
                    twoFields1 (fields2 "notempty" "notempty") "key2" "key3" (passwordMatch PasswordNotEqual success) (valid Required)
                        |> Expect.equal (valid (Required "notempty"))
            , test "fails if the first field is missing" <|
                \_ ->
                    twoFields1 (fields2 "" "") "notfound1" "key3" (passwordMatch PasswordNotEqual success) (valid Required)
                        |> Expect.equal (formE "notfound1" missingField)
            , test "fails if the second field is missing" <|
                \_ ->
                    twoFields1 (fields2 "" "") "key2" "notfound2" (passwordMatch PasswordNotEqual success) (valid Required)
                        |> Expect.equal (formE "notfound2" missingField)
            , test "fails if both fields are missing" <|
                \_ ->
                    twoFields1 (fields2 "" "") "notfound1" "notfound2" (passwordMatch PasswordNotEqual success) (valid Required)
                        |> Expect.equal (formEL "notfound1" "notfound2" missingField)
            , test "fails on both fields if the validation fails" <|
                \_ ->
                    twoFields1 (fields2 "a" "b") "key2" "key3" (passwordMatch PasswordNotEqual success) (valid Required)
                        |> Expect.equal (formEL "key2" "key3" (CustomErr PasswordNotEqual))
            ]
        , describe "Validation.fieldgroup1"
            [ fuzz string "helps validating a FieldGroup (bind)" <|
                \s ->
                    fieldgroup1 (fields3 s) "group1" (\f -> required f "key4" (stringField success) (valid Required)) (valid FieldGroup)
                        |> Expect.equal (valid (FieldGroup (Required s)))
            , test "fails if the group is missing" <|
                \_ ->
                    fieldgroup1 (fields3 "") "notfound" (\_ -> valid (Required "")) (valid FieldGroup)
                        |> Expect.equal (formE "notfound" missingField)
            , test "fails if the group validation fails" <|
                \_ ->
                    fieldgroup1 (fields3 "notaa") "group1" (\f -> required f "key4" (stringField valFail) (valid Required)) (valid FieldGroup)
                        |> Expect.equal (formE "key4" (CustomErr "error"))
            ]
        ]



-- Fixutres


missingField : FieldError err
missingField =
    ConfigErr MissingField


wrongType : FieldError err
wrongType =
    ConfigErr WrongType


type TestError
    = NotChecked
    | NotInt
    | NotFloat
    | EmptyString
    | WrongLength
    | NotEmail
    | PasswordNotEqual


fields1 : String -> F.Fields String
fields1 s =
    F.fields
        [ ( "key1", F.inputWithDefault s ) ]


fields2 : String -> String -> F.Fields String
fields2 s1 s2 =
    F.fields
        [ ( "key2", F.inputWithDefault s1 )
        , ( "key3", F.inputWithDefault s2 )
        ]


fields3 : String -> F.Fields String
fields3 s =
    F.fields
        [ ( "group1"
          , F.group
                [ ( "key4", F.inputWithDefault s ) ]
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


type alias SomeResult =
    { a : String
    , b : String
    , c : String
    }


fieldsValidate1 : String -> F.Fields String
fieldsValidate1 s =
    F.fields
        [ ( "a", F.inputWithDefault s )
        , ( "b", F.inputWithDefault s )
        , ( "c", F.inputWithDefault s )
        ]


fieldsValidate2 : F.Fields String
fieldsValidate2 =
    F.fields
        [ ( "a", F.input )
        , ( "b", F.checkbox )
        , ( "c", F.input )
        ]


validate1 : Validate String String SomeResult
validate1 fields =
    valid SomeResult
        |> required fields "a" (stringField <| success)
        |> required fields "b" (stringField <| success)
        |> required fields "c" (stringField <| success)


validate2 : Validate String String SomeResult
validate2 fields =
    valid SomeResult
        |> required fields "notfound" (stringField <| success)
        |> required fields "b" (stringField <| success)
        |> required fields "c" (stringField <| success)


validate3 : String -> Validate String String SomeResult
validate3 s fields =
    valid SomeResult
        |> required fields "a" (stringField <| \_ -> failure s)
        |> required fields "b" (stringField <| success)
        |> required fields "c" (stringField <| success)


validate4 : String -> Validate String String SomeResult
validate4 s fields =
    valid SomeResult
        |> required fields "a" (stringField <| \_ -> failure s)
        |> required fields "b" (stringField <| \_ -> failure s)
        |> required fields "c" (stringField <| success)



-- Helpers


formE : comparable -> FieldError err -> FormValidation comparable err a
formE comparable fe =
    VA.failure (FormError comparable fe)


formEL : comparable -> comparable -> FieldError err -> FormValidation comparable err a
formEL comparable1 comparable2 fe =
    VA.Failure (VA.ErrorList [ FormError comparable1 fe, FormError comparable2 fe ])


valFail : String -> FieldValidation String String
valFail =
    validation "error" ((==) "aa")
