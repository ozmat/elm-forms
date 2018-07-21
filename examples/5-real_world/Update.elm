module Update exposing (..)

import Api
import Forms.Form as F
import Forms.Update as FU
import Forms.Validation.Result as FV
import Model exposing (..)
import Msg exposing (..)


-- Update function


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- `RegisterForm` messages
        RegisterForm formMsg ->
            let
                newForm =
                    -- Update the form : one form field has changed and
                    -- we need to update the form
                    FU.updateForm formMsg model.registerForm

                newResult =
                    -- Validate the form and store the new result
                    -- in order to be able to show the last state
                    -- of the form in the 'Result' view (debug)
                    F.validate newForm

                newErrors =
                    -- Pull out the errors (easier to display them in the view)
                    case newResult of
                        FV.Invalid err ->
                            Just err

                        _ ->
                            Nothing

                newModel =
                    { model
                        | registerForm = newForm
                        , registerResult = Just newResult
                        , registerErrors = newErrors
                    }
            in
            -- Define some `stringFieldCommands`: we want more control over
            -- the "typicode-user" select field
            FU.stringFieldCommands newModel formMsg registerFormCommands

        -- "We got some typicode-users" message
        TypicodeUsersComplete newUsers ->
            let
                users =
                    -- We could keep the `List TypicodeUser` here but
                    -- we're not going to use them so a `List (String, String)`
                    -- is easier to use in the view (form selects)
                    typicodeUserSelect newUsers

                cmd =
                    -- If we have at least one user,
                    -- retrieve the first user posts
                    case List.head users of
                        Nothing ->
                            Cmd.none

                        Just ( userId, _ ) ->
                            Api.getTypicodePosts userId
            in
            ( { model
                | typicodeUsers = users

                -- Remove the previous posts
                , typicodePosts = []
              }
            , cmd
            )

        -- "We got some typicode-posts" message
        TypicodePostsComplete newPosts ->
            let
                posts =
                    -- Same than with "typicode-users",
                    -- a `List (String, String)` is enough for this example
                    typicodePostSelect newPosts

                newModel =
                    -- Now that we have new posts we need to set the
                    -- form field value to the first post because it's going
                    -- to be the one selected in the view (first option)
                    case List.head posts of
                        Nothing ->
                            model

                        Just ( postId, _ ) ->
                            { model | registerForm = F.setStringField "typicode-post" postId model.registerForm }
            in
            ( { newModel
                | typicodePosts = posts

                -- Re-validate the form and store the new result
                -- in order to keep the 'Result' view up-to-date (debug)
                , registerResult = Just (F.validate newModel.registerForm)
              }
            , Cmd.none
            )

        -- "Something wrong happened with typicode" message
        TypicodeFailed ->
            -- Disable the API feature if any API call fails (better to
            -- have a working example)
            ( { model
                | brokenApi = True
                , typicodeUsers = []
                , typicodePosts = []
              }
            , Cmd.none
            )



-- `RegisterForm` side effects


registerFormCommands : Model -> String -> String -> ( Model, Cmd Msg )
registerFormCommands model key value =
    case key of
        -- When a new "typicode-user" is selected, we need to fetch
        -- their "typicode-posts"
        "typicode-user" ->
            ( { model
                -- Remove the previous posts
                | typicodePosts = []

                -- Ensure the current "typicode-post" value is null
                -- in case something goes wrong before we set
                -- the "typicode-post" value to the first new post
                , registerForm = F.setStringField "typicode-post" "" model.registerForm
              }
              -- Fetch the "typicode-posts"
            , Api.getTypicodePosts value
            )

        _ ->
            ( model
            , Cmd.none
            )
