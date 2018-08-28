module Forms.Field exposing
    ( Field, Fields
    , fields, input, select, checkbox, group
    , inputWithDefault, selectWithDefault, checkboxWithDefault
    )

{-| `Field` represents a [`Form`](http://package.elm-lang.org/packages/ozmat/elm-forms/latest/Forms-Form#Form) field


# Fields

@docs Field, Fields


# Creation

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

@docs fields, input, select, checkbox, group


# Using a default value

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


{-| A `Field` can be :

  - an input field
  - a select field
  - a checkbox field
  - or a fieldgroup

It is looked-up using `comparable` so a `Field String` uses a `String` as
field key

-}
type alias Field comparable =
    Internal.Field comparable


{-| `Fields` regroups several `Field` and represents all your form fields.
-}
type alias Fields comparable =
    Internal.Fields comparable



-- Creation


{-| Creates an input `Field` with the [default string `Value`](http://package.elm-lang.org/packages/ozmat/elm-forms/latest/Forms-Value#defaultString)
-}
input : Field comparable
input =
    FieldValue V.defaultString


{-| Creates a select `Field` with the [default string `Value`](http://package.elm-lang.org/packages/ozmat/elm-forms/latest/Forms-Value#defaultString)
-}
select : Field comparable
select =
    input


{-| Creates a checkbox `Field` with the [default bool `Value`](http://package.elm-lang.org/packages/ozmat/elm-forms/latest/Forms-Value#defaultBool)
-}
checkbox : Field comparable
checkbox =
    FieldValue V.defaultBool


{-| Creates a fieldgroup using a `List`
-}
group : List ( comparable, Field comparable ) -> Field comparable
group g =
    FieldGroup (fields g)


{-| Creates a `Fields` using a `List`. This is the top-level function when
creating form fields
-}
fields : List ( comparable, Field comparable ) -> Fields comparable
fields =
    D.fromList



-- Using a default value


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
