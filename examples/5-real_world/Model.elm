module Model exposing (..)

import Dict exposing (Dict)
import Forms as F exposing (Form)
import Forms.Field as FF exposing (Fields)
import Forms.Validation as FV exposing (Validate)
import Forms.Validation.Result exposing (FormResult)
import Json.Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (decode, required)


{- Model -}


type alias Model =
    { -- The register form
      registerForm : Form String RegisterError OurUser

    -- The register form result : we only store this one
    -- for the 'Result' view (debug)
    , registerResult : Maybe (FormResult String RegisterError OurUser)

    -- The register form errors : this one is for convenience. It's easier to
    -- display the errors in the view if we already have access to the errors
    , registerErrors : Maybe (Dict String RegisterError)

    -- Is the typicode-API broken ?
    , brokenApi : Bool

    -- The current typicode-users
    , typicodeUsers : List ( String, String )

    -- And the current typicode-posts
    , typicodePosts : List ( String, String )
    }


initModel : Model
initModel =
    { registerForm = F.form registerFields registerValidate
    , registerResult = Nothing
    , registerErrors = Nothing
    , brokenApi = False
    , typicodeUsers = []
    , typicodePosts = []
    }



{- Result types of the form -}


type alias OurUser =
    { createdFrom : CreatedFrom
    , firstName : String
    , lastName : String
    , email : String
    , encryptedPassword : String
    , gender : Maybe Gender
    , jobProfile : JobProfile
    , favoritePostId : Maybe Int
    , newsletter : Bool
    }


type CreatedFrom
    = Mobile
    | FrontEnd
    | Partner


encryptPassword : String -> String
encryptPassword _ =
    "do_some_encryption"


type Gender
    = Man
    | Woman


toGender : String -> Maybe Gender
toGender s =
    case s of
        "man" ->
            Just Man

        "woman" ->
            Just Woman

        _ ->
            Nothing


type alias JobProfile =
    { company : Maybe String
    , salaryAfterTax : Float
    }



{- Register Form -}


type RegisterError
    = EmptyString
    | NotInt
    | NotEmail
    | PasswordNotMatching
    | PasswordLength
    | PasswordNoStarChar
    | NotFloat
    | TermsNotAccepted


registerFields : Fields String
registerFields =
    FF.fields
        [ ( "first-name", FF.input )
        , ( "last-name", FF.input )
        , ( "email", FF.input )
        , ( "password", FF.input )
        , ( "password-repeat", FF.input )
        , ( "gender", FF.select )
        , ( "job-profile"
          , FF.group
                [ ( "company", FF.input )
                , ( "salary", FF.input )
                ]
          )
        , ( "typicode-user", FF.select )
        , ( "typicode-post", FF.select )
        , ( "newsletter", FF.checkboxWithDefault True )
        , ( "terms", FF.checkbox )
        ]


registerValidate : Validate String RegisterError OurUser
registerValidate fields =
    let
        -- You can define those validation functions outside of
        -- the `Validate` function for reusability/clarity
        nameValidation =
            -- Nonempty String
            -- String to lower case
            FV.notEmpty EmptyString (String.toLower >> FV.success)

        securePassword =
            -- Password length : 6 > length > 20
            -- Password contains a '*'
            -- Encrypt password
            FV.length 6 21 PasswordLength <|
                \s ->
                    if String.contains "*" s then
                        FV.success (encryptPassword s)
                    else
                        FV.failure PasswordNoStarChar
    in
    FV.valid OurUser
        -- Harcode the `FrontEnd` value
        |> FV.hardcoded FrontEnd
        -- Use `nameValidation`
        |> FV.required fields "first-name" (FV.stringField <| nameValidation)
        |> FV.required fields "last-name" (FV.stringField <| nameValidation)
        -- Make sure it's an `email`
        |> FV.required fields "email" (FV.stringField <| FV.email NotEmail FV.success)
        -- Make sure it's a `securePassword` that matches with "password-repeat"
        |> FV.twoFields fields "password" "password-repeat" (FV.passwordMatch PasswordNotMatching securePassword)
        -- This one is `optional` with `Nothing` as a default value.
        -- If we have a value we'll try to convert it `toGender`
        |> FV.optional fields "gender" Nothing (toGender >> FV.success)
        -- Nested `JobProfile` type
        |> FV.fieldgroup fields "job-profile" jobProfileValidate
        -- This one is an `optionalWithMaybe` (`Nothing` as default, `Just`
        -- otherwise) that can be cast into an `Int`
        |> FV.optionalWithMaybe fields "typicode-post" (FV.int NotInt <| FV.success)
        -- Basic `required`
        |> FV.required fields "newsletter" (FV.boolField <| FV.success)
        -- This one is `discardable` : we need to validate it but we can discard
        -- the result. Here we just make sure that the checkbox is checked.
        |> FV.discardable fields "terms" (FV.boolField <| FV.isChecked TermsNotAccepted FV.success)


jobProfileValidate : FV.Validate String RegisterError JobProfile
jobProfileValidate fields =
    let
        removeTax =
            -- Subtract 20% of the value
            (*) 0.8 >> FV.success
    in
    FV.valid JobProfile
        -- Basic `optionalWithMaybe`
        |> FV.optionalWithMaybe fields "company" FV.success
        -- `optional` (with 0.0 as default value) that can be cast into
        -- a `Float` to which we're going to `removeTax`
        |> FV.optional fields "salary" 0.0 (FV.float NotFloat <| removeTax)



{- API types -}


type alias TypicodeUser =
    { userId : Int
    , name : String
    }


typicodeUserDecoder : Decoder TypicodeUser
typicodeUserDecoder =
    decode TypicodeUser
        |> required "id" int
        |> required "name" string


typicodeUsersDecoder : Decoder (List TypicodeUser)
typicodeUsersDecoder =
    list typicodeUserDecoder


typicodeUserSelect : List TypicodeUser -> List ( String, String )
typicodeUserSelect =
    List.map (\user -> ( toString user.userId, user.name ))


type alias TypicodePost =
    { postId : Int
    , title : String
    }


typicodePostDecoder : Decoder TypicodePost
typicodePostDecoder =
    decode TypicodePost
        |> required "id" int
        |> required "title" string


typicodePostsDecoder : Decoder (List TypicodePost)
typicodePostsDecoder =
    list typicodePostDecoder


typicodePostSelect : List TypicodePost -> List ( String, String )
typicodePostSelect =
    List.map (\post -> ( toString post.postId, post.title ))



{- View data -}


genderSelectOptions : List ( String, String )
genderSelectOptions =
    [ ( "", "Prefer not to disclose" )
    , ( "man", "Man" )
    , ( "woman", "Woman" )
    ]



-- errorToString : Lang -> RegisterError -> String
-- errorToString lang error =
--     case lang of
--         EN ->
--             case error of
--                 EmptyString ->
--                     "This field must not be empty"
--                 NotInt ->
--                     "Please provide a valid integer"
--                 NotEmail ->
--                     "Please provide a valid email"
--                 ...
--         ES ->
--             ...
