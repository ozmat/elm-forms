module Main exposing (..)

import Api
import Html exposing (program)
import Model exposing (Model, initModel)
import Msg exposing (Msg)
import Update exposing (update)
import View exposing (view)


{- Main -}


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }



{- Init state -}


init : ( Model, Cmd Msg )
init =
    ( initModel
      -- Fetch the typicode-users on startup
    , Api.getTypicodeUsers
    )
