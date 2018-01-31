module Ki.Validation exposing (..)

import Ki.Field as F exposing (Group)
import Ki.Value as V exposing (Value)


{- Generic implementation of validation -}
-- TODO Move the generic impl into another library ?
-- Error


type ValidationError err
    = Error err
    | ErrorList (List err)


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


mapError : (err1 -> err2) -> ValidationError err1 -> ValidationError err2
mapError f ve =
    case ve of
        Error err1 ->
            Error (f err1)

        ErrorList l ->
            ErrorList (List.map f l)


toList : ValidationError err -> List err
toList ve =
    case ve of
        Error e ->
            [ e ]

        ErrorList l ->
            l



-- Validation


type Validation err a
    = Failure (ValidationError err)
    | Success a


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


map : (a -> b) -> Validation err a -> Validation err b
map f validation =
    case validation of
        Success a ->
            Success (f a)

        Failure ve ->
            Failure ve


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



{- Concrete usage of validation for Form -}
-- Field validation
-- TODO refactor "valid"
-- TODO side-effect select validation ??


type FieldError err
    = MissingField
    | WrongType
    | CustomError err


type alias FieldValidation err a =
    Validation (FieldError err) a


customFailure : err -> FieldValidation err a
customFailure err =
    failure (CustomError err)


validF : a -> FieldValidation err a
validF =
    success


fieldValidation : err -> (a -> Bool) -> a -> FieldValidation err a
fieldValidation err valid a =
    validation (CustomError err) valid a


missingField : (Value -> FieldValidation err a) -> Maybe Value -> FieldValidation err a
missingField valid mvalue =
    case mvalue of
        Nothing ->
            failure MissingField

        Just value ->
            valid value



-- TODO Use a Type FieldType ? To replace stringField, boolField and detail WrongType
-- TODO Add int parsing and float parsing
-- TODO Add basic validation (email, password, length)


stringField : (String -> FieldValidation err a) -> Value -> FieldValidation err a
stringField valid value =
    case value of
        V.String s ->
            valid s

        _ ->
            failure WrongType


boolField : (Bool -> FieldValidation err a) -> Value -> FieldValidation err a
boolField valid value =
    case value of
        V.Bool b ->
            valid b

        _ ->
            failure WrongType



-- Form validation


type FormError comparable err
    = FormError comparable (FieldError err)


type alias FormValidation comparable err a =
    Validation (FormError comparable err) a


mapFormError : comparable -> FieldValidation err a -> FormValidation comparable err a
mapFormError comparable fv =
    case fv of
        Failure fe ->
            Failure (mapError (FormError comparable) fe)

        Success a ->
            Success a



{- Validate a Form -}


type alias Validate comparable err a =
    Group comparable -> FormValidation comparable err a


valid : a -> FormValidation comparable err a
valid =
    success



{- Validate a Field -}
-- TODO implement the fieldGroup validation


fieldValid : Group comparable -> comparable -> (Value -> FieldValidation err a) -> FormValidation comparable err a
fieldValid fields comparable valid =
    mapFormError comparable (missingField valid (F.getValue comparable fields))



-- Required


required : Group comparable -> comparable -> (Value -> FieldValidation err a) -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
required fields comparable valid fvf =
    andMap (fieldValid fields comparable valid) fvf


requiredAcc : Group comparable -> comparable -> (Value -> FieldValidation err a) -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
requiredAcc fields comparable valid fvf =
    andMapAcc (fieldValid fields comparable valid) fvf



-- Hardcoded, equivalent to :
-- ```|> required fields comparable (\_ -> valid a)```


harcoded : Group comparable -> comparable -> a -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
harcoded fields comparable a fvf =
    andMap (fieldValid fields comparable (\_ -> validF a)) fvf


harcodedAcc : Group comparable -> comparable -> a -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
harcodedAcc fields comparable a fvf =
    andMapAcc (fieldValid fields comparable (\_ -> validF a)) fvf



-- Optional, equivalent to :
-- a "string only required" that returns default if empty or validates the string otherwise
-- TODO implement optional for more than just String


optional_ : (String -> FieldValidation err a) -> a -> (Value -> FieldValidation err a)
optional_ svalid default =
    \value ->
        stringField
            (\s ->
                if String.isEmpty s then
                    validF default
                else
                    svalid s
            )
            value


optional : Group comparable -> comparable -> (String -> FieldValidation err a) -> a -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
optional fields comparable valid default fvf =
    andMap (fieldValid fields comparable (optional_ valid default)) fvf


optionalAcc : Group comparable -> comparable -> (String -> FieldValidation err a) -> a -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
optionalAcc fields comparable valid default fvf =
    andMapAcc (fieldValid fields comparable (optional_ valid default)) fvf



-- OptionalMaybe, equivalent to :
-- ```|> optional fields comparable (\s -> ... Just s) Nothing```


optionalMaybe : Group comparable -> comparable -> (String -> FieldValidation err a) -> FormValidation comparable err (Maybe a -> b) -> FormValidation comparable err b
optionalMaybe fields comparable valid fvf =
    optional fields comparable (\s -> map Just (valid s)) Nothing fvf


optionalMaybeAcc : Group comparable -> comparable -> (String -> FieldValidation err a) -> FormValidation comparable err (Maybe a -> b) -> FormValidation comparable err b
optionalMaybeAcc fields comparable valid fvf =
    optionalAcc fields comparable (\s -> map Just (valid s)) Nothing fvf



{- Validate a Group -}


missingGroup : (Group comparable -> FormValidation comparable err a) -> comparable -> Maybe (Group comparable) -> FormValidation comparable err a
missingGroup valid comparable mgroup =
    case mgroup of
        Nothing ->
            mapFormError comparable (failure MissingField)

        Just value ->
            valid value


groupValid : Group comparable -> comparable -> (Group comparable -> FormValidation comparable err a) -> FormValidation comparable err a
groupValid fields comparable valid =
    missingGroup valid comparable (F.getGroup comparable fields)


fieldGroup : Group comparable -> comparable -> (Group comparable -> FormValidation comparable err a) -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
fieldGroup fields comparable valid fvf =
    andMap (groupValid fields comparable valid) fvf


fieldGroupAcc : Group comparable -> comparable -> (Group comparable -> FormValidation comparable err a) -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
fieldGroupAcc fields comparable valid fvf =
    andMapAcc (groupValid fields comparable valid) fvf
