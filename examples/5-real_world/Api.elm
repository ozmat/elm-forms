module Api exposing (..)

import Http
import HttpBuilder exposing (..)
import Model exposing (..)
import Msg exposing (..)
import Time


-- URL


typicodeApiUrl : String
typicodeApiUrl =
    "http://jsonplaceholder.typicode.com"



-- API


getTypicodeUsers : Cmd Msg
getTypicodeUsers =
    HttpBuilder.get (typicodeApiUrl ++ "/users")
        |> withTimeout (10 * Time.second)
        |> withExpect (Http.expectJson typicodeUsersDecoder)
        |> send typicodeUsersComplete


typicodeUsersComplete : Result Http.Error (List TypicodeUser) -> Msg
typicodeUsersComplete result =
    case result of
        Ok users ->
            TypicodeUsersComplete users

        Err _ ->
            TypicodeFailed


getTypicodePosts : String -> Cmd Msg
getTypicodePosts userId =
    HttpBuilder.get (typicodeApiUrl ++ "/posts")
        |> withQueryParams [ ( "userId", userId ) ]
        |> withTimeout (10 * Time.second)
        |> withExpect (Http.expectJson typicodePostsDecoder)
        |> send typicodePostsComplete


typicodePostsComplete : Result Http.Error (List TypicodePost) -> Msg
typicodePostsComplete result =
    case result of
        Ok posts ->
            TypicodePostsComplete posts

        Err _ ->
            TypicodeFailed
