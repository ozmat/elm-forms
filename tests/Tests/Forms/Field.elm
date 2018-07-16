module Tests.Forms.Field exposing (..)

import Dict as D
import Expect
import Forms.Field as F exposing (..)
import Forms.Field.Internal as IF exposing (Field(..))
import Forms.Value as V
import Fuzz as F
import Test exposing (..)


all : Test
all =
    describe "Forms.Field tests"
        [ test "Field.input creates a default String FieldValue" <|
            \_ ->
                input
                    |> Expect.equal (FieldValue V.defaultString)
        , test "Field.select creates a default String FieldValue" <|
            \_ ->
                select
                    |> Expect.equal (FieldValue V.defaultString)
        , test "Field.checkbox creates a default Bool FieldValue" <|
            \_ ->
                checkbox
                    |> Expect.equal (FieldValue V.defaultBool)
        , test "Field.group creates a FieldGroup from a tuple-list" <|
            \_ ->
                group fixture1
                    |> Expect.equal (FieldGroup (D.fromList fixture1))
        , test "Field.fields creates a Fields from a tuple-list" <|
            \_ ->
                fields fixture1
                    |> Expect.equal (D.fromList fixture1)
        , fuzz F.string "Field.inputWithDefault creates a String FieldValue" <|
            \s ->
                inputWithDefault s
                    |> Expect.equal (FieldValue (V.string s))
        , fuzz F.string "Field.selectWithDefault creates a String FieldValue" <|
            \s ->
                selectWithDefault s
                    |> Expect.equal (FieldValue (V.string s))
        , fuzz F.bool "Field.checkboxWithDefault creates a Bool FieldValue" <|
            \b ->
                checkboxWithDefault b
                    |> Expect.equal (FieldValue (V.bool b))
        , describe "Field.getValue"
            [ fuzz F.string "retrieves a value from a Fields (depth 1)" <|
                \s ->
                    IF.getValue "key1" (fixture2 s)
                        |> Expect.equal (Just (V.string s))
            , fuzz F.string "retrieves a value from a Fields (depth 2)" <|
                \s ->
                    IF.getValue "key2" (fixture2 s)
                        |> Expect.equal (Just (V.string s))
            , fuzz F.string "retrieves a value from a Fields (depth 3)" <|
                \s ->
                    IF.getValue "key3" (fixture2 s)
                        |> Expect.equal (Just (V.string s))
            , test "returns Nothing otherwise" <|
                \_ ->
                    IF.getValue "notfound" (fixture2 "")
                        |> Expect.equal Nothing
            ]
        , describe "Field.getGroup"
            [ fuzz F.string "retrieves a Fields from a Fields" <|
                \s ->
                    IF.getGroup "group1" (fixture2 s)
                        |> Expect.equal (Just (fixture3 s))
            , test "returns Nothing otherwise" <|
                \_ ->
                    IF.getGroup "notfound" (fixture2 "")
                        |> Expect.equal Nothing
            ]
        , describe "Field.setValue"
            [ fuzz F.string "set a value in a Fields (depth 1)" <|
                \s ->
                    IF.getValue "key1" (IF.setValue "key1" (V.string s) (fixture2 ""))
                        |> Expect.equal (Just (V.string s))
            , fuzz F.string "set a value in a Fields (depth 2)" <|
                \s ->
                    IF.getValue "key2" (IF.setValue "key2" (V.string s) (fixture2 ""))
                        |> Expect.equal (Just (V.string s))
            , fuzz F.string "set a value in a Fields (depth 3)" <|
                \s ->
                    IF.getValue "key3" (IF.setValue "key3" (V.string s) (fixture2 ""))
                        |> Expect.equal (Just (V.string s))
            , fuzz F.string "has no effect if the key does not exist" <|
                \s ->
                    IF.setValue "notfound" (V.string s) (fixture2 "")
                        |> Expect.equal (fixture2 "")
            , test "has no effect if the Value has a different type" <|
                \_ ->
                    IF.setValue "key1" V.defaultBool (fixture2 "")
                        |> Expect.equal (fixture2 "")
            ]
        ]



-- Fixtures


fixture1 : List ( String, F.Field String )
fixture1 =
    [ ( "key1", input )
    , ( "key2", checkbox )
    ]


fixture2 : String -> Fields String
fixture2 s =
    fields
        [ ( "key1", inputWithDefault s )
        , ( "group1", FieldGroup (fixture3 s) )
        ]


fixture3 : String -> Fields String
fixture3 s =
    fields
        [ ( "key2", inputWithDefault s )
        , ( "group2"
          , group
                [ ( "key3", inputWithDefault s ) ]
          )
        ]
