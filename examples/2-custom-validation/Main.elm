module Main exposing (..)

import Html exposing (Html, program, text, div, input)
import Html.Attributes exposing (placeholder, style)
import Html.Events exposing (onInput)
import Debug exposing (log)


-- Forms import

import Forms.Field as FF
import Forms.Validation as FV
import Forms.Form as F
import Forms.Update as FU


{- MAIN -}


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , subscriptions = (\_ -> Sub.none)
        , view = view
        }



{- Model -}


init : ( Model, Cmd Msg )
init =
    ( Model (F.form myFormFields myFormValidate)
    , Cmd.none
    )


type alias Model =
    { myForm : F.Form String MyFormError OtherModel
    }



{- Form -}


type alias OtherModel =
    { firstName : String
    , lastName : String
    , age : Int
    , userModel : UserModel
    , wallet : Float
    , color : Maybe String
    }


type alias UserModel =
    { email : String
    , password : String
    , userType : UserType
    }


type UserType
    = User
    | Admin


myFormFields : FF.Group String
myFormFields =
    FF.fields
        [ ( "first_name", FF.string )
        , ( "last_name", FF.string )
        , ( "age", FF.string )
        , ( "user_group"
          , FF.group
                [ ( "email", FF.string )
                , ( "password", FF.string )
                , ( "password_again", FF.string )
                ]
          )
        , ( "wallet", FF.string )
        , ( "color", FF.string )
        ]


type MyFormError
    = TooYoung
    | TooOld


myFormValidate : FV.Validate String MyFormError OtherModel
myFormValidate fields =
    FV.valid OtherModel
        |> FV.required fields "first_name" (FV.stringValid <| FV.notEmpty <| FV.success << String.toLower << String.trim)
        |> FV.required fields "last_name" (FV.stringValid <| FV.notEmpty <| FV.success << String.toLower << String.trim)
        |> FV.required fields
            "age"
            (FV.stringValid <|
                FV.int <|
                    \i ->
                        if i < 18 then
                            FV.customFailure TooYoung
                        else if i > 35 then
                            FV.customFailure TooOld
                        else
                            FV.success i
            )
        |> FV.fieldGroup fields "user_group" myUserValide


myUserValide : FV.Validate String MyFormError UserModel
myUserValide fields =
    FV.valid UserModel
        |> FV.required fields "email" (FV.stringValid <| FV.email <| FV.success << String.toLower << String.trim)
        |> FV.twoFields fields "password" "password_again" FV.passwordMatch



{- Msg -}


type Msg
    = Form (FU.Msg String)



{- Update -}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Form formMsg ->
            let
                newModel =
                    { model | myForm = FU.updateForm formMsg model.myForm }

                console =
                    log "" (F.validate newModel.myForm)
            in
                newModel ! []



{- View -}


view : Model -> Html Msg
view model =
    div []
        [ inputText "First Name" "first_name"
        , inputText "Last Name" "last_name"
        , inputText "Age" "age"
        , inputText "Email" "email"
        , inputText "Password" "password"
        , inputText "Type again your password" "password_again"
        , inputText "Wallet" "wallet"
        , inputText "Color" "color"
        ]


inputText : String -> String -> Html Msg
inputText placeHolder fieldName =
    let
        inputStyle =
            style
                [ ( "width", "100%" )
                , ( "height", "40px" )
                , ( "padding", "10px 0" )
                , ( "font-size", "2em" )
                , ( "text-align", "center" )
                ]
    in
        input
            [ inputStyle
            , placeholder placeHolder
            , onInput (Form << FU.UpdateStringField fieldName)
            ]
            []
