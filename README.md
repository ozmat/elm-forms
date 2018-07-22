# elm-forms

A library for building and validating [`Forms`](http://package.elm-lang.org/packages/ozmat/elm-forms/latest/Forms-Form#Form) in Elm

[![Build Status](https://travis-ci.org/ozmat/elm-forms.svg?branch=master)](https://travis-ci.org/ozmat/elm-forms)

## Motivation

There is no specific way of handling/validating forms in Elm. This library aims
to provide an easy but flexible way of creating forms, validating them, 
handling the different message updates and building a whole business logic 
on top of them.

Here is a list of different features that we need when dealing with forms imo 
(and that the library actually implements) :

- **Custom error**  
   We should be able to fail with custom errors but also implement some logic
   on our errors : a custom error is usually more than a string (but can be 
   just a string if that's enough)

- **Field error**  
   We should be able to have errors at the field level in order to identify
   which field caused the failure during the validation process

- **Field identification**  
   The key associated with a field should be more than a string : a comparable
   
- **Validation process**  
   The validation process should be accumulative and gather all the errors. But
   should also provide a way to stop and fail at the first error encountered.

- **View agnostic**  
   We should be able to work with forms whether we use Bootstrap, MDL, 
   elm-lang/html ... The library should only provide functions to use in the
   view layer

Plus, the library should also be :

- **Easy to use**  
   And provide helper functions dealing with the complex stuff so we can 
   focus on the business logic

- **Flexible**  
   Enough to fit most of the common use cases. But also give us the 
   opportunity to do more if we understand what's happening under the hood

- **Pipeline friendly**  
  Because when you start using [elm-decode-pipeline](http://package.elm-lang.org/packages/NoRedInk/elm-decode-pipeline/latest) 
  you just really like this syntax :)

## A quick overview

Here are some parts of the [first example](https://github.com/ozmat/elm-forms/tree/master/examples/1-readme_example) : a basic form with three fields (two 
strings and one int) that creates an `OtherModel`

```elm
...

{- Model -}

init : ( Model, Cmd Msg )
init =
    ( Model (F.form myFormFields myFormValidate)
    , Cmd.none
    )


type alias Model =
    { myForm : F.Form String MyFormError OtherModel
    }

...

{- Form -}

type alias OtherModel =
    { firstName : String
    , lastName : String
    , referenceNumber : Int
    }


type MyFormError
    = EmptyString
    | NotInt


myFormFields : FF.Fields String
myFormFields =
    FF.fields
        [ ( "first-name", FF.input )
        , ( "last-name", FF.input )
        , ( "reference-number", FF.input )
        ]


myFormValidate : FV.Validate String MyFormError OtherModel
myFormValidate fields =
    FV.valid OtherModel
        |> FV.required fields "first-name" (FV.stringField <| FV.notEmpty EmptyString FV.success)
        |> FV.required fields "last-name" (FV.stringField <| FV.notEmpty EmptyString FV.success)
        |> FV.required fields "reference-number" (FV.stringField <| FV.int NotInt FV.success)

...

{- Msg/Update -}

type Msg
    = Form (FU.Msg String)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Form formMsg ->
          { model | myForm = FU.updateForm formMsg model.myForm } ! []

...

{- View -}

inputText : String -> String -> Html Msg
inputText placeHolder fieldName =
    input
      [ inputStyle
      , placeholder placeHolder
      , onInput (FU.stringFieldMsg Form fieldName)
      ]
      []

...

F.validate model.myForm -- validates the form and returns a `FormResult`
```

## A journey with Forms (examples)

For a better understanding on how to use the library, I would recommend you
to go through all the [examples](https://github.com/ozmat/elm-forms/tree/master/examples), following the order specified. You should
have a look at the source code, as there are some comments there, while running 
the examples and playing with them.

All the examples are single apps (and have their own `elm-package.json`),
so you can run them like this :

```sh
cd EXAMPLE_DIR
elm-reactor
```

If you have downloaded/cloned the all library you can just :

```sh
cd elm-forms/examples
elm-reactor
```

and have access to all the examples.

## Troubleshooting

- If you have a [`ConfigError`](http://package.elm-lang.org/packages/ozmat/elm-forms/latest/Forms-Validation-Result#ConfigError)
when validating your form, have a look at which field is concerned and
    - If the error is a `MissingField` one : make sure there is no typo error
    in the field key either in the [`Fields`](http://package.elm-lang.org/packages/ozmat/elm-forms/latest/Forms-Field#Fields)
    or in the [`Validate`](http://package.elm-lang.org/packages/ozmat/elm-forms/latest/Forms-Validation#Validate) function
    - If the error is a `WrongType` one : make sure you're using the
    [`stringField`](http://package.elm-lang.org/packages/ozmat/elm-forms/latest/Forms-Validation#stringField) helper
    for an [`input`](http://package.elm-lang.org/packages/ozmat/elm-forms/latest/Forms-Field#input)/[`select`](http://package.elm-lang.org/packages/ozmat/elm-forms/latest/Forms-Field#select) field
    and the [`boolField`](http://package.elm-lang.org/packages/ozmat/elm-forms/latest/Forms-Validation#boolField) helper
    for a [`checkbox`](http://package.elm-lang.org/packages/ozmat/elm-forms/latest/Forms-Field#checkbox) field
- If a field value never changes
    - First make sure you're implementing the form [`Msg`](http://package.elm-lang.org/packages/ozmat/elm-forms/latest/Forms-Update#Msg) 
    and you're updating the form with the [`updateForm`](http://package.elm-lang.org/packages/ozmat/elm-forms/latest/Forms-Update#updateForm) function
    - Second, in your view, make sure you have an event defined on the field 
    and it's using a form `Msg` : [`stringFieldMsg`](http://package.elm-lang.org/packages/ozmat/elm-forms/latest/Forms-Update#stringFieldMsg) 
    or [`boolFieldMsg`](http://package.elm-lang.org/packages/ozmat/elm-forms/latest/Forms-Update#boolFieldMsg)
    - Finally make sure there is no typo error in the field key either in the 
    [`Fields`](http://package.elm-lang.org/packages/ozmat/elm-forms/latest/Forms-Field#Fields) 
    or in your view event
- If a field validation never/always fails/succeeds, make sure you're using
the [`success`](http://package.elm-lang.org/packages/ozmat/elm-forms/latest/Forms-Validation#success) 
or [`failure`](http://package.elm-lang.org/packages/ozmat/elm-forms/latest/Forms-Validation#failure) function

