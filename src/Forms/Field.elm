module Forms.Field
    exposing
        ( Field
        , Fields
        , checkbox
        , checkboxWithDefault
        , fields
        , group
        , input
        , inputWithDefault
        , select
        , selectWithDefault
        )

{-| A `Field` represents a [`Form`](http://package.elm-lang.org/packages/ozmat/elm-forms/latest/Forms-Form#Form) field


# Definition

@docs Field, Fields


# Creation

@docs input, select, checkbox, group, fields

    myFormFields : Fields String
    myFormFields =
        fields
            [ ( "some-input", input )
            , ( "some-select", select )
            , ( "some-checkbox", checkbox )
            , ( "some-group",
              , group
                    [ ( "some-other-field", input)
                    , ...
                    ]
              )
            ]


# Changing default value

    myFormFields : Fields String
    myFormFields =
        fields
            [ ( "some-input", inputWithDefault "Initial value" )
            , ( "some-select", selectWithDefault "Default choice" )
            , ( "some-checkbox", checkboxWithDefault True )
            ]

@docs inputWithDefault, selectWithDefault, checkboxWithDefault

-}

import Dict as D exposing (Dict)
import Forms.Field.Internal as Internal exposing (Field(..))
import Forms.Value as V


{-| A `Field` can either hold a `Value` (`FieldValue`) or
a group of `Field`s (`FieldGroup`)
-}
type alias Field comparable =
    Internal.Field comparable


{-| `Fields` is a group of `Field`s. The underlying data structure is a [`Dict`](http://package.elm-lang.org/packages/elm-lang/core/latest/Dict#Dict)
-}
type alias Fields comparable =
    Internal.Fields comparable



-- Creation


{-| Creates an input `Field` with the [default `String` `Value`](http://package.elm-lang.org/packages/ozmat/elm-forms/latest/Forms-Value#defaultString)
-}
input : Field comparable
input =
    FieldValue V.defaultString


{-| Creates a select `Field` with the [default `String` `Value`](http://package.elm-lang.org/packages/ozmat/elm-forms/latest/Forms-Value#defaultString)
-}
select : Field comparable
select =
    input


{-| Creates a checkbox `Field` with the [default `Bool` `Value`](http://package.elm-lang.org/packages/ozmat/elm-forms/latest/Forms-Value#defaultBool)
-}
checkbox : Field comparable
checkbox =
    FieldValue V.defaultBool


{-| Creates a `FieldGroup` : a `Field` that holds a group of `Field`s
-}
group : List ( comparable, Field comparable ) -> Field comparable
group g =
    FieldGroup (fields g)


{-| Creates a group of `Field`s. This is the top-level function when creating
`Field`s.
-}
fields : List ( comparable, Field comparable ) -> Fields comparable
fields =
    D.fromList



-- Changing default value


{-| Creates an input `Field` with a default value
-}
inputWithDefault : String -> Field comparable
inputWithDefault s =
    FieldValue (V.string s)


{-| Creates a select `Field` with a default value
-}
selectWithDefault : String -> Field comparable
selectWithDefault =
    inputWithDefault


{-| Creates a checkbox `Field` with a default value
-}
checkboxWithDefault : Bool -> Field comparable
checkboxWithDefault b =
    FieldValue (V.bool b)
