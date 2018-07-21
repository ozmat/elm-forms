module Forms.Update.Internal
    exposing
        ( Msg(..)
        )

-- Form Messages


type Msg comparable
    = UpdateStringField comparable String
    | UpdateBoolField comparable Bool
