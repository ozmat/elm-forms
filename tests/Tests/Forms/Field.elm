module Tests.Forms.Field exposing (..)

import Dict as D
import Expect
import Test exposing (..)
import Fuzz as F
import Forms.Field exposing (..)
import Forms.Value as V


all : Test
all =
    describe "Forms.Field tests"
        [ test "Field.string creates a default String FieldValue" <|
            \_ ->
                string
                    |> Expect.equal (FieldValue (V.defaultString))
        , test "Field.bool creates a default Bool FieldValue" <|
            \_ ->
                bool
                    |> Expect.equal (FieldValue (V.defaultBool))
        , test "Field.group creates a FieldGroup from a tuple-list" <|
            \_ ->
                group fixture1
                    |> Expect.equal (FieldGroup (D.fromList fixture1))
        , test "Field.fields creates a Group from a tuple-list" <|
            \_ ->
                fields fixture1
                    |> Expect.equal (D.fromList fixture1)
        , fuzz F.string "Field.stringWithValue creates a String FieldValue" <|
            \s ->
                stringWithValue s
                    |> Expect.equal (FieldValue (V.string s))
        , fuzz F.bool "Field.boolWithValue creates a Bool FieldValue" <|
            \b ->
                boolWithValue b
                    |> Expect.equal (FieldValue (V.bool b))
        , describe "Field.getValue"
            [ fuzz F.string "retrieves a value from a Group (depth 1)" <|
                \s ->
                    getValue "key1" (fixture2 s)
                        |> Expect.equal (Just (V.string s))
            , fuzz F.string "retrieves a value from a Group (depth 2)" <|
                \s ->
                    getValue "key2" (fixture2 s)
                        |> Expect.equal (Just (V.string s))
            , fuzz F.string "retrieves a value from a Group (depth 3)" <|
                \s ->
                    getValue "key3" (fixture2 s)
                        |> Expect.equal (Just (V.string s))
            , test "returns Nothing otherwise" <|
                \_ ->
                    getValue "notfound" (fixture2 "")
                        |> Expect.equal Nothing
            ]
        , describe "Field.getGroup"
            [ fuzz F.string "retrieves a Group from a Group" <|
                \s ->
                    getGroup "group1" (fixture2 s)
                        |> Expect.equal (Just (fixture3 s))
            , test "returns Nothing otherwise" <|
                \_ ->
                    getGroup "notfound" (fixture2 "")
                        |> Expect.equal Nothing
            ]
        , describe "Field.setValue"
            [ fuzz F.string "set a value in a Group (depth 1)" <|
                \s ->
                    getValue "key1" (setValue "key1" (V.string s) (fixture2 ""))
                        |> Expect.equal (Just (V.string s))
            , fuzz F.string "set a value in a Group (depth 2)" <|
                \s ->
                    getValue "key2" (setValue "key2" (V.string s) (fixture2 ""))
                        |> Expect.equal (Just (V.string s))
            , fuzz F.string "set a value in a Group (depth 3)" <|
                \s ->
                    getValue "key3" (setValue "key3" (V.string s) (fixture2 ""))
                        |> Expect.equal (Just (V.string s))
            , fuzz F.string "has no effect if the key does not exist" <|
                \s ->
                    setValue "notfound" (V.string s) (fixture2 "")
                        |> Expect.equal (fixture2 "")
            , test "has no effect if the Value has a different type" <|
                \_ ->
                    setValue "key1" (V.defaultBool) (fixture2 "")
                        |> Expect.equal (fixture2 "")
            ]
        ]



-- Fixtures


fixture1 : List ( String, Field String )
fixture1 =
    [ ( "key1", string )
    , ( "key2", bool )
    ]


fixture2 : String -> Group String
fixture2 s =
    fields
        [ ( "key1", stringWithValue s )
        , ( "group1", FieldGroup (fixture3 s) )
        ]


fixture3 : String -> Group String
fixture3 s =
    fields
        [ ( "key2", stringWithValue s )
        , ( "group2"
          , group
                [ ( "key3", stringWithValue s ) ]
          )
        ]