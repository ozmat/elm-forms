module Msg exposing (..)

import Forms.Update as FU
import Model exposing (TypicodePost, TypicodeUser)


type Msg
    = -- RegisterForm messages
      RegisterForm (FU.Msg String)
      -- Submit button message
    | SubmitResgisterForm
      -- "Close the modal" message
    | CloseModal
      -- "We got some typicode-users" message
    | TypicodeUsersComplete (List TypicodeUser)
      -- "We got some typicode-posts" message
    | TypicodePostsComplete (List TypicodePost)
      -- "Something wrong happened with typicode" message
    | TypicodeFailed
