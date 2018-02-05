# elm-forms

A library for building and validating [`Forms`](http://package.elm-lang.org/packages/ozmat/elm-forms/latest/Forms-Form#Form) in Elm

[![Build Status](https://travis-ci.org/ozmat/elm-forms.svg?branch=master)](https://travis-ci.org/ozmat/elm-forms)

## Motivation

There is no specific way of handling/validating Forms in Elm. This library aims
to provide an easy but flexible way of generating Forms, validating them, 
handling the different message updates and build a whole business logic on top
of them.

Here is a list of different features that we need when dealing with Forms imo 
(and that the library actually implements) :

- **Custom error**

   We should be able to fail with custom errors but also implement some logic
   on our errors : a custom error is usually more than a String but can be 
   just a String if that's enough

- **Field error**

   We should be able to have errors at the field level in order to identify
   which field caused the failure during the validation process

- **Field identification**

   The key associated with a field should be more than a String : a comparable
   
- **Validation process**

   The validation process should be accumulative and gather all the errors. But
   can also provide a way to stop and fail at the first error encountered.

- **View agnostic**

   We should be able to work with Forms whether we use Bootstrap, MDL, 
   elm-lang/html ... The library should only provide function to use in the
   view layer

Plus, the library should also be :

- **Easy to use**
   
   And provide helper functions dealing with the complex stuff so we can 
   focus on the business logic

- **Flexible**
   
   Enough to fit most of the common use cases. But we should also be able 
   to do more than just what the helpers give us (if we understand what's 
   happening under the hood and we want to fully enjoy the existing code)

- **Pipeline friendly**

  Because when you start using [elm-decode-pipeline](http://package.elm-lang.org/packages/NoRedInk/elm-decode-pipeline/latest) 
  you just really like this syntax :)

## Basic example

In this example we have a basic Form with three fields. The validation process
will make sure that we get two Strings, one Int and that we construct an
`OtherModel` if we succeed.

```elm
import Forms.Field as FF
import Forms.Validation as FV
import Forms.Form as F
import Forms.Update as FU

...

{- Model -}

init : ( Model, Cmd Msg )
init =
    ( Model (F.form myFormFields myFormValidate)
    , Cmd.none
    )


type alias Model =
    { myForm : F.Form String () OtherModel
    }

{- Form -}

type alias OtherModel =
    { firstName : String
    , lastName : String
    , referenceNumber : Int
    }

myFormFields : FF.Group String
myFormFields =
    FF.fields
        [ ( "first_name", FF.string )
        , ( "last_name", FF.string )
        , ( "reference_number", FF.string )
        ]

myFormValidate : FV.Validate String () OtherModel
myFormValidate fields =
    FV.valid OtherModel
        |> FV.required fields "first_name" (FV.stringValid <| FV.notEmpty <| FV.success)
        |> FV.required fields "last_name" (FV.stringValid <| FV.notEmpty <| FV.success)
        |> FV.required fields "reference_number" (FV.stringValid <| FV.int <| FV.success)

{- Msg -}

type Msg
    = Form (FU.Msg String)

{- Update -}

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Form formMsg ->
          { model | myForm = FU.updateForm formMsg model.myForm } ! []

{- View -}

inputText : String -> String -> Html Msg
inputText placeHolder fieldName =
    input
      [ inputStyle
      , placeholder placeHolder
      , onInput (Form << FU.UpdateStringField fieldName)
      ]
      []

...

F.validate newModel.myForm -- validates the form and returns a Result

```

## Advanced examples

If you want to see more examples using the library have a look in the 
[source code](https://github.com/ozmat/elm-forms/tree/master/examples)