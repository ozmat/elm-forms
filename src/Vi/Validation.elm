module Vi.Validation
    exposing
        ( -- Validation error
          ValidationError(..)
        , append
        , toList
        , errorMap
          -- Validation
        , Validation(..)
        , failure
        , success
        , validation
        , map
        , mapError
        , mapValidationError
        , andMap
        , andMapAcc
        )

{- Validation error -}


type ValidationError err
    = Error err
    | ErrorList (List err)



-- Helpers


append : ValidationError err -> ValidationError err -> ValidationError err
append ve1 ve2 =
    case ( ve1, ve2 ) of
        ( Error err1, Error err2 ) ->
            ErrorList [ err1, err2 ]

        ( Error err, ErrorList l ) ->
            ErrorList (err :: l)

        ( ErrorList l, Error err ) ->
            ErrorList (err :: l)

        ( ErrorList l1, ErrorList l2 ) ->
            ErrorList (l1 ++ l2)


toList : ValidationError err -> List err
toList ve =
    case ve of
        Error e ->
            [ e ]

        ErrorList l ->
            l



-- Map


errorMap : (err1 -> err2) -> ValidationError err1 -> ValidationError err2
errorMap f ve =
    case ve of
        Error err1 ->
            Error (f err1)

        ErrorList l ->
            ErrorList (List.map f l)



{- Validation -}


type Validation err a
    = Failure (ValidationError err)
    | Success a



-- Helpers


failure : err -> Validation err a
failure err =
    Failure (Error err)


success : a -> Validation err a
success a =
    Success a


validation : err -> (a -> Bool) -> a -> Validation err a
validation err valid a =
    if valid a then
        Success a
    else
        Failure (Error err)



-- Map


map : (a -> b) -> Validation err a -> Validation err b
map f validation =
    case validation of
        Success a ->
            Success (f a)

        Failure ve ->
            Failure ve


mapError : (err1 -> err2) -> Validation err1 a -> Validation err2 a
mapError f validation =
    mapValidationError (errorMap f) validation


mapValidationError : (ValidationError err1 -> ValidationError err2) -> Validation err1 a -> Validation err2 a
mapValidationError f validation =
    case validation of
        Success a ->
            Success a

        Failure ve ->
            Failure (f ve)



-- AndMap


andMap : Validation err a -> Validation err (a -> b) -> Validation err b
andMap va vf =
    case ( va, vf ) of
        ( _, Failure ve ) ->
            Failure ve

        ( _, Success f ) ->
            map f va


andMapAcc : Validation err a -> Validation err (a -> b) -> Validation err b
andMapAcc va vf =
    case ( va, vf ) of
        ( Success _, Failure ve ) ->
            Failure ve

        ( Failure ve1, Failure ve2 ) ->
            Failure (append ve1 ve2)

        ( _, Success f ) ->
            map f va
