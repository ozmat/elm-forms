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
    -- Compare to the previous example the form result has more fields/records
    { firstName : String
    , lastName : String
    , age : Int

    -- And also a nested type (`UserModel`)
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
        [ ( "first-name", FF.input )
        , ( "last-name", FF.input )
        , ( "age", FF.input )

        -- We use a `Field.group` here to regroup all the fields of
        -- the `UserModel` nested type
        , ( "user-group"
          , FF.group
                [ ( "email", FF.input )
                , ( "password", FF.input )
                , ( "password-again", FF.input )
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
        -- This time we still want the "first-name" to be not empty but
        -- we also want to add some post-validation process that will `trim`
        -- and `toUpper` the string
        |> FV.required fields "first-name" (FV.stringField <| FV.notEmpty EmptyString <| FV.success << String.toUpper << String.trim)
        -- Same than the previous one except that we want a `toLower` string
        |> FV.required fields "last-name" (FV.stringField <| FV.notEmpty EmptyString <| FV.success << String.toLower << String.trim)
        -- For the "age" we want a string that can be cast into an `Int`
        |> FV.required fields
            "age"
            (FV.stringField <|
                FV.int NotInt <|
                    \i ->
                        if i < 18 then
                            -- But we also need the people to be at least 18
                            -- otherwise we fail with a `TooYoung` error
                            FV.failure TooYoung
                        else if i > 35 then
                            -- And not older than 35
                            -- otherwise we fail with a `TooOld` error
                            FV.failure TooOld
                        else
                            FV.success i
            )
        -- Here we want to validate the `UserModel` nested type
        -- so we use a `fieldgroup` (on the fieldgroup key) and a separate
        -- validate function (for clarity)
        |> FV.fieldgroup fields "user-group" myUserValidate
        -- The "wallet" is `optional` : this means that if the field is empty,
        -- we will use the specified default value (here 0).
        -- If it's not empty, we want a string (`optional` only works with
        -- `stringField`s so we don't need to specify it) that can be cast
        -- into a `Float`
        |> FV.optional fields "wallet" 0 (FV.float NotFloat FV.success)
        -- The "color" is also optional but using a `Maybe` : if the field is
        -- empty the value will be `Nothing` and `Just` otherwise.
        -- The "color" needs to be a string (same than with `optional`,
        -- `optionalWithMaybe` only works with `stringField`s so no need to
        -- specify it) that `isColor`. Here we're using the `validation` helper
        -- that creates the validation function for us. It is useful when you
        -- only need to fail with one error and you have a boolean test.
        -- Compare to the "age", where we needed to fail with 2 errors,
        -- the "color" can either be a color or not.
        |> FV.optionalWithMaybe fields "color" (FV.validation WrongColor isColor)


myUserValidate : FV.Validate String MyFormError UserModel
myUserValidate fields =
    -- Notice that the `Validate` type is about a `UserModel` this time.
    -- We could actually have another form only creating a `UserModel`
    -- and reuse this `Validate` function.
    FV.valid UserModel
        -- The "email" field needs to be a string and is validated using
        -- the `email` helper. Then we post `trim` and `toLower` the string.
        |> FV.required fields "email" (FV.stringField <| FV.email NotEmail <| FV.success << String.toLower << String.trim)
        -- We're using `twoFields` here to validate the "password". `twoFields`
        -- is useful when you have one field depending on another one but
        -- you only need to store one result. We're also using the
        -- `passwordMatch` helper that makes sure that the two fields match.
        -- And finally we want to post `encryptPassword`.
        |> FV.twoFields fields "password" "password-again" (FV.passwordMatch PasswordDontMatch <| FV.success << encryptPassword)
        -- The last field is not part of the form but only of the `UserModel`.
        -- We're using the `hardcoded` helper that helps harcoding a value
        -- during the validation process
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
        [ inputText "First Name" "first-name"
        , inputText "Last Name" "last-name"
        , inputText "Age" "age"
        , inputText "Email" "email"
        , inputText "Password" "password"
        , inputText "Type again your password" "password-again"
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
        , onInput (FU.stringFieldMsg Form fieldName)
        ]
        []
