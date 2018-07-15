module Main exposing (..)

import Debug exposing (log)
import Forms.Field as FF
import Forms.Form as F
import Forms.Update as FU
import Forms.Validation as FV
import Html exposing (Html, div, input, program, text)
import Html.Attributes exposing (placeholder, style)
import Html.Events exposing (onInput)


{- MAIN -}


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
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


isColor : String -> Bool
isColor s =
    List.member s [ "blue", "red" ]


type alias UserModel =
    { email : String
    , password : String
    , userType : UserType
    }


encryptPassword : String -> String
encryptPassword _ =
    "@encrypted@"


type UserType
    = User
    | Admin


myFormFields : FF.Fields String
myFormFields =
    FF.fields
        [ ( "first_name", FF.input )
        , ( "last_name", FF.input )
        , ( "age", FF.input )
        , ( "user_group"
          , FF.group
                [ ( "email", FF.input )
                , ( "password", FF.input )
                , ( "password_again", FF.input )
                ]
          )
        , ( "wallet", FF.input )
        , ( "color", FF.input )
        ]


type MyFormError
    = EmptyString
    | NotInt
    | TooYoung
    | TooOld
    | NotFloat
    | WrongColor
    | NotEmail
    | PasswordDontMatch


myFormValidate : FV.Validate String MyFormError OtherModel
myFormValidate fields =
    FV.valid OtherModel
        |> FV.required fields "first_name" (FV.stringField <| FV.notEmpty EmptyString <| FV.success << String.toUpper << String.trim)
        |> FV.required fields "last_name" (FV.stringField <| FV.notEmpty EmptyString <| FV.success << String.toLower << String.trim)
        |> FV.required fields
            "age"
            (FV.stringField <|
                FV.int NotInt <|
                    \i ->
                        if i < 18 then
                            FV.failure TooYoung
                        else if i > 35 then
                            FV.failure TooOld
                        else
                            FV.success i
            )
        |> FV.fieldgroup fields "user_group" myUserValidate
        |> FV.optional fields "wallet" 0 (FV.float NotFloat FV.success)
        |> FV.optionalWithMaybe fields "color" (FV.validation WrongColor isColor)


myUserValidate : FV.Validate String MyFormError UserModel
myUserValidate fields =
    FV.valid UserModel
        |> FV.required fields "email" (FV.stringField <| FV.email NotEmail <| FV.success << String.toLower << String.trim)
        |> FV.twoFields fields "password" "password_again" (FV.passwordMatch PasswordDontMatch <| FV.success << encryptPassword)
        |> FV.hardcoded User



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
