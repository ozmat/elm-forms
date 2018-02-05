module Forms.Field
    exposing
        ( -- Field
          Field(..)
        , Group
          -- Helpers
        , string
        , bool
        , group
        , fields
        , stringWithValue
        , boolWithValue
          -- Getters and Setter
        , getValue
        , getGroup
        , setValue
        )

{-| A `Field` represents a [`Form`](http://package.elm-lang.org/packages/ozmat/elm-forms/latest/Forms-Form#Form) field.
Please refer to the [examples](https://github.com/ozmat/elm-forms/tree/master/examples) for a better understanding


# Definition

@docs Field, Group


# Common Helpers

@docs string, bool, group, fields, stringWithValue, boolWithValue


# Search

@docs getValue, getGroup, setValue

-}

import Dict as D exposing (Dict)
import Forms.Value as V exposing (Value)


{-| A `Field` can be a simple field with a `Value` (= `FieldValue`) or a
group of fields (= `FieldGroup`)

    FieldValue (String "some input value")

    FieldGroup (Dict.fromList [ ... ])

-}
type Field comparable
    = FieldGroup (Group comparable)
    | FieldValue Value


{-| A `Group` is a group of `Field`s. The underlying data structure is a [`Dict`](http://package.elm-lang.org/packages/elm-lang/core/latest/Dict#Dict)
-}
type alias Group comparable =
    Dict comparable (Field comparable)



-- Common Helpers


{-| Is a shortcut to create a `Field` with the [default `String` `Value`](http://package.elm-lang.org/packages/ozmat/elm-forms/latest/Forms-Value#defaultString)

    string -- FieldValue (Forms.Value.defaultString)

-}
string : Field comparable
string =
    FieldValue V.defaultString


{-| Is a shortcut to create a `Field` with the [default `Bool` `Value`](http://package.elm-lang.org/packages/ozmat/elm-forms/latest/Forms-Value#defaultBool)

    bool -- FieldValue (Forms.Value.defaultBool)

-}
bool : Field comparable
bool =
    FieldValue V.defaultBool


{-| Is a shortcut to create a `Field` with a `Group` of fields. The function
takes a `List` of `Tuple` and creates the `Group` for you :

    tupleExample : ( comparable, Field comparable )
    tupleExample =
        (comparable, string)

    group [tupleExample, ...] -- FieldGroup (Dict.fromList [ ... ])

-}
group : List ( comparable, Field comparable ) -> Field comparable
group g =
    FieldGroup (fields g)


{-| Is a shortcut to create a `Group` of `Field`s. The function takes a
`List` of `Tuple`

    someFormFields : Group comparable
    someFormFields =
        fields
            [ ( comparable1, string )
            , ( comparable2, bool )
            , ( comparable3
              , group
                    [ ( comparable4, string )
                    , ( comparable5, string )
                    , ( comparable6, string )
                    ]
              )
            ]

-}
fields : List ( comparable, Field comparable ) -> Group comparable
fields =
    D.fromList


{-| Is a shortcut to create a `Field` with an initial `String` `Value`

    stringWithValue "initial" -- FieldValue (String "initial")

-}
stringWithValue : String -> Field comparable
stringWithValue s =
    FieldValue (V.string s)


{-| Is a shortcut to create a `Field` with an initial `Bool` `Value`

    boolWithValue True -- FieldValue (Bool True)

-}
boolWithValue : Bool -> Field comparable
boolWithValue b =
    FieldValue (V.bool b)



-- Map


mapValue : (Value -> Value) -> Field comparable -> Field comparable
mapValue f field =
    case field of
        FieldValue v ->
            FieldValue (f v)

        _ ->
            field


mapGroup : (Group comparable -> Group comparable) -> Field comparable -> Field comparable
mapGroup f field =
    case field of
        FieldGroup g ->
            FieldGroup (f g)

        _ ->
            field



-- Update


updateValue : Value -> Maybe (Field comparable) -> Maybe (Field comparable)
updateValue value field =
    Maybe.map (mapValue (V.safeUpdate value)) field


updateGroup : Group comparable -> Maybe (Field comparable) -> Maybe (Field comparable)
updateGroup group field =
    Maybe.map (mapGroup (always group)) field



-- Group traversal -> get


getField : comparable -> Group comparable -> Maybe (Field comparable)
getField comparable group =
    let
        walk k v acc =
            case acc of
                Just _ ->
                    acc

                Nothing ->
                    case v of
                        FieldGroup g ->
                            getField comparable g

                        _ ->
                            Nothing
    in
        case D.get comparable group of
            Just field ->
                Just field

            Nothing ->
                D.foldl walk Nothing group


{-| Retrieves the `Value` associated with a key. If the key is not found or
the `Field` is not a `FieldValue` returns `Nothing`

    someFormFields : Group comparable
    someFormFields =
        fields
            [ ( comparable1, string )
            , ( comparable2, bool )
            , ( comparable3, group [] )
            ]

    getValue comparable1 someFormFields -- Just (String ...)
    getValue comparable2 someFormFields -- Just (Bool ...)
    getValue comparable3 someFormFields -- Nothing
    getValue notfound someFormFields    -- Nothing

-}
getValue : comparable -> Group comparable -> Maybe Value
getValue comparable group =
    case getField comparable group of
        Just field ->
            case field of
                FieldValue value ->
                    Just value

                _ ->
                    Nothing

        Nothing ->
            Nothing


{-| Retrieves the `Group` associated with a key. If the key is not found or
the `Field` is not a `FieldGroup` returns `Nothing`

    someFormFields : Group comparable
    someFormFields =
        fields
            [ ( comparable1, string )
            , ( comparable2, bool )
            , ( comparable3, group [] )
            ]

    getGroup comparable1 someFormFields -- Nothing
    getGroup comparable2 someFormFields -- Nothing
    getGroup comparable3 someFormFields -- Just (Dict.fromList [ ... ])
    getGroup notfound someFormFields    -- Nothing

-}
getGroup : comparable -> Group comparable -> Maybe (Group comparable)
getGroup comparable group =
    case getField comparable group of
        Just field ->
            case field of
                FieldGroup g ->
                    Just g

                _ ->
                    Nothing

        Nothing ->
            Nothing



-- Group traversal -> set


{-| Updates the `Value` associated with a key. It will only update the `Value`
if the key is found, the `Field` is a `FieldValue` and the `Value`s have the
same type

    someFormFields : Group comparable
    someFormFields =
        fields
            [ ( comparable1, string )
            , ( comparable2, group [] )
            ]

    setValue comparable1 (String ...) someFormFields -- updates
    setValue comparable1 (Bool ...) someFormFields   -- doesn't update
    setValue comparable2 (...) someFormFields        -- doesn't update
    setValue notfound (...) someFormFields           -- doesn't update

-}
setValue : comparable -> Value -> Group comparable -> Group comparable
setValue comparable value group =
    let
        walk k v acc =
            if k == comparable then
                D.update k (updateValue value) acc
            else
                case v of
                    FieldGroup g ->
                        D.update k (updateGroup (setValue comparable value g)) acc

                    _ ->
                        acc
    in
        D.foldl walk group group
