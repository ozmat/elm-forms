module Tests.Forms.Value exposing (all)

import Expect
import Forms.Value exposing (..)
import Forms.Value.Internal as IV exposing (Value(..))
import Fuzz as F
import Test exposing (..)


all : Test
all =
    describe "Forms.Value tests"
        [ fuzz F.string "Value.string creates a String Value" <|
            \s ->
                string s
                    |> Expect.equal (String s)
        , fuzz F.bool "Value.bool creates a Bool Value" <|
            \b ->
                bool b
                    |> Expect.equal (Bool b)
        , test "Value.defaultString creates a default String Value" <|
            \_ ->
                defaultString
                    |> Expect.equal (String "")
        , fuzz F.bool "Value.defaultBool creates a default Bool Value" <|
            \b ->
                defaultBool
                    |> Expect.equal (Bool False)
        , describe "Value.getString"
            [ fuzz F.string "returns the value of a String Value" <|
                \s ->
                    getString (String s)
                        |> Expect.equal (Just s)
            , test "returns Nothing otherwise" <|
                \_ ->
                    getString (Bool True)
                        |> Expect.equal Nothing
            ]
        , describe "Value.isBool"
            [ fuzz F.bool "returns the value of a Bool Value" <|
                \b ->
                    getBool (Bool b)
                        |> Expect.equal (Just b)
            , test "returns Nothing otherwise" <|
                \_ ->
                    getBool (String "")
                        |> Expect.equal Nothing
            ]
        , describe "Value.safeUpdate"
            [ test "updates value of same type" <|
                \_ ->
                    let
                        updated =
                            String "updated"
                    in
                    IV.safeUpdate updated defaultString
                        |> Expect.equal updated
            , test "has no effect on value of different type" <|
                \_ ->
                    IV.safeUpdate defaultString defaultBool
                        |> Expect.equal defaultBool
            ]
        ]
