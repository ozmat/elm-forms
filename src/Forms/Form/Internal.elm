module Forms.Form.Internal
    exposing
        ( Form(..)
        )

import Forms.Field.Internal exposing (Fields)
import Forms.Validation.Internal exposing (Validate)


{-| A `Form` is made up of `Fields` and a `Validate` function
-}
type Form comparable err a
    = Form (Fields comparable) (Validate comparable err a)
