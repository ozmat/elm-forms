module Tests.Forms.Value exposing (..)

import Expect
import Test exposing (..)
import Fuzz as F
import Forms.Value exposing (..)


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
        , describe "Value.safeUpdate"
            [ test "updates value of same type" <|
                \_ ->
                    let
                        updated =
                            String "updated"
                    in
                        safeUpdate updated defaultString
                            |> Expect.equal updated
            , test "has no effect on value of different type" <|
                \_ ->
                    safeUpdate defaultString defaultBool
                        |> Expect.equal defaultBool
            ]
        ]
