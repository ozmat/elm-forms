module View exposing (..)

import Bootstrap.CDN as CDN
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Fieldset as Fieldset
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Select as Select
import Bootstrap.Grid as Grid
import Bootstrap.Text as Text
import Dict
import Forms.Form as F
import Forms.Update as FU
import Html exposing (..)
import Html.Attributes exposing (..)
import Model as M exposing (Model)
import Msg exposing (..)


-- Top level view


view : Model -> Html Msg
view model =
    div []
        [ CDN.stylesheet -- not good for real-world usage
        , mainGrid model
        ]



-- Main grid


mainGrid : Model -> Html Msg
mainGrid model =
    Grid.container
        [ style
            [ ( "margin-top", "30px" )
            , ( "margin-bottom", "30px" )
            ]
        ]
        [ Grid.row []
            [ Grid.col []
                [ formView model ]
            , Grid.col []
                [ resultView model ]
            ]
        ]



-- Result View


resultView : Model -> Html Msg
resultView model =
    Card.config [ Card.outlineInfo, Card.attrs [ class "sticky-top" ] ]
        |> Card.header []
            [ text "Result" ]
        |> Card.block [ Block.textColor Text.info ]
            [ Block.text []
                [ text (toString model.registerResult) ]
            ]
        |> Card.view



-- Form view


formView : Model -> Html Msg
formView model =
    Form.form []
        [ formGroup <|
            inputField "first-name" "First Name" False model
        , formGroup <|
            inputField "last-name" "Last Name" False model
        , formGroup <|
            inputField "email" "Email address" False model
        , formGroup <|
            inputField "password" "Password" True model
        , formGroup <|
            inputField "password-repeat" "Repeat password" True model
        , formGroup <|
            selectField "gender" "Gender" M.genderSelectOptions <|
                Just "We'll never share this with anyone else."
        , fieldSet False "Job Profile" <|
            [ fieldSetGroup <|
                inputField "company" "Company name" False model
            , fieldSetGroup <|
                inputField "salary" "Salary" False model
            ]
        , fieldSet model.brokenApi "Favorite Post" <|
            [ fieldSetGroup <|
                selectField "typicode-user" "Author" model.typicodeUsers Nothing
            , fieldSetGroup <|
                selectField "typicode-post" "Post" model.typicodePosts Nothing
            ]
        , checkboxField "newsletter" "Newsletter subscription" model
        , checkboxField "terms" "Accept our Terms" model
        ]



-- form view components


formGroup : List (Html Msg) -> Html Msg
formGroup =
    Form.group []


inputField : String -> String -> Bool -> Model -> List (Html Msg)
inputField inputId title isPassword model =
    let
        label =
            Form.label
                [ for inputId ]
                [ text title ]

        inputOptions =
            [ Input.id inputId
            , Input.onInput (FU.stringFieldMsg RegisterForm inputId)
            ]

        inputType =
            if isPassword then
                Input.password
            else
                Input.text

        input =
            case hasError inputId model of
                Nothing ->
                    [ inputType inputOptions ]

                Just e ->
                    [ inputType (Input.danger :: inputOptions)
                    , Form.invalidFeedback [] [ text e ]
                    ]
    in
    label :: input


selectField : String -> String -> List ( String, String ) -> Maybe String -> List (Html Msg)
selectField selectId title options mhelp =
    let
        help =
            case mhelp of
                Nothing ->
                    []

                Just help ->
                    [ Form.help [] [ text help ] ]
    in
    [ Form.label
        [ for selectId ]
        [ text title ]
    , Select.select
        [ Select.id selectId
        , Select.onChange (FU.stringFieldMsg RegisterForm selectId)
        ]
        (List.map
            (\( v, t ) -> Select.item [ value v ] [ text t ])
            options
        )
    ]
        ++ help


checkboxField : String -> String -> Model -> Html Msg
checkboxField checkboxId title model =
    let
        baseOptions =
            [ Checkbox.id checkboxId
            , Checkbox.checked (isChecked checkboxId model)
            , Checkbox.onCheck (FU.boolFieldMsg RegisterForm checkboxId)
            ]

        options =
            case hasError checkboxId model of
                Just _ ->
                    Checkbox.danger :: baseOptions

                Nothing ->
                    baseOptions
    in
    Checkbox.checkbox
        options
        title


fieldSet : Bool -> String -> List (Html Msg) -> Html Msg
fieldSet isDisabled title content =
    Fieldset.config
        |> Fieldset.disabled isDisabled
        |> Fieldset.legend [] [ text title ]
        |> Fieldset.children content
        |> Fieldset.view


fieldSetGroup : List (Html Msg) -> Html Msg
fieldSetGroup content =
    Fieldset.config
        |> Fieldset.asGroup
        |> Fieldset.children content
        |> Fieldset.view



-- form view helpers


hasError : String -> Model -> Maybe String
hasError inputId model =
    model.registerErrors
        |> Maybe.andThen (Dict.get inputId)
        -- Have a look at Model.errorToString and use
        -- that kind of function instead of toString here
        |> Maybe.andThen (Just << toString)


isChecked : String -> Model -> Bool
isChecked checkboxId model =
    case F.getBoolField checkboxId model.registerForm of
        Just b ->
            b

        Nothing ->
            False
