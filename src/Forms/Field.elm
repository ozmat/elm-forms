module Forms.Field
    exposing
        ( -- Field
          Field(..)
        , Fields
          -- Helpers
        , input
        , select
        , checkbox
        , group
        , fields
        , inputWithDefault
        , selectWithDefault
        , checkboxWithDefault
          -- Getters and Setter
        , getValue
        , getGroup
        , setValue
        )

{-| A `Field` represents a [`Form`](http://package.elm-lang.org/packages/ozmat/elm-forms/latest/Forms-Form#Form) field.
Please refer to the [examples](https://github.com/ozmat/elm-forms/tree/master/examples) for a better understanding


# Definition

@docs Field, Fields


# Common Helpers

@docs input, select, checkbox, group, fields


# Default value

@docs inputWithDefault, selectWithDefault, checkboxWithDefault


# Search

@docs getValue, getGroup, setValue

-}

import Dict as D exposing (Dict)
import Forms.Value as V exposing (Value)


{-| A `Field` can either hold a `Value` (= `FieldValue`) or a
group of `Field`s (= `FieldGroup`)

    FieldValue (String "some input value")

    FieldGroup (Dict.fromList [ ... ])

-}
type Field comparable
    = FieldValue Value
    | FieldGroup (Fields comparable)


{-| `Fields` is a group of `Field`s. The underlying data structure is a [`Dict`](http://package.elm-lang.org/packages/elm-lang/core/latest/Dict#Dict)
-}
type alias Fields comparable =
    Dict comparable (Field comparable)



-- Common Helpers


{-| Is a shortcut to create an input `Field` with the [default `String` `Value`](http://package.elm-lang.org/packages/ozmat/elm-forms/latest/Forms-Value#defaultString)

    input -- FieldValue (Forms.Value.defaultString)

-}
input : Field comparable
input =
    FieldValue V.defaultString


{-| Is a shortcut to create a select `Field` with the [default `String` `Value`](http://package.elm-lang.org/packages/ozmat/elm-forms/latest/Forms-Value#defaultString)

    select -- FieldValue (Forms.Value.defaultString)

-}
select : Field comparable
select =
    input


{-| Is a shortcut to create a checkbox `Field` with the [default `Bool` `Value`](http://package.elm-lang.org/packages/ozmat/elm-forms/latest/Forms-Value#defaultBool)

    checkbox -- FieldValue (Forms.Value.defaultBool)

-}
checkbox : Field comparable
checkbox =
    FieldValue V.defaultBool


{-| Is a shortcut to create a `Field` with a group of `Field`s. The function
takes a `List` of `Tuple` and creates the `Fields` for you :

    tupleExample : ( comparable, Field comparable )
    tupleExample =
        (comparable, string)

    group [tupleExample, ...] -- FieldGroup (Dict.fromList [ ... ])

-}
group : List ( comparable, Field comparable ) -> Field comparable
group g =
    FieldGroup (fields g)


{-| Is a shortcut to create `Fields`. The function takes a
`List` of `Tuple`

    someFormFields : Fields comparable
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
fields : List ( comparable, Field comparable ) -> Fields comparable
fields =
    D.fromList



-- Default value


{-| Is a shortcut to create an input `Field` with a default value

    inputWithDefault "initial" -- FieldValue (String "initial")

-}
inputWithDefault : String -> Field comparable
inputWithDefault s =
    FieldValue (V.string s)


{-| Is a shortcut to create a select `Field` with a default value

    selectWithDefault "initial" -- FieldValue (String "initial")

-}
selectWithDefault : String -> Field comparable
selectWithDefault =
    inputWithDefault


{-| Is a shortcut to create a checkbox `Field` with a default value

    checkboxWithDefault True -- FieldValue (Bool True)

-}
checkboxWithDefault : Bool -> Field comparable
checkboxWithDefault b =
    FieldValue (V.bool b)



-- Map


mapValue : (Value -> Value) -> Field comparable -> Field comparable
mapValue f field =
    case field of
        FieldValue v ->
            FieldValue (f v)

        _ ->
            field


mapGroup : (Fields comparable -> Fields comparable) -> Field comparable -> Field comparable
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


updateGroup : Fields comparable -> Maybe (Field comparable) -> Maybe (Field comparable)
updateGroup group field =
    Maybe.map (mapGroup (always group)) field



-- Fields traversal -> get


getField : comparable -> Fields comparable -> Maybe (Field comparable)
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

    someFormFields : Fields comparable
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
getValue : comparable -> Fields comparable -> Maybe Value
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


{-| Retrieves the group of `Field`s associated with a key. If the key is not found or
the `Field` is not a `FieldGroup` returns `Nothing`

    someFormFields : Fields comparable
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
getGroup : comparable -> Fields comparable -> Maybe (Fields comparable)
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



-- Fields traversal -> set


{-| Updates the `Value` associated with a key. It will only update the `Value`
if the key is found, the `Field` is a `FieldValue` and the `Value`s have the
same type

    someFormFields : Fields comparable
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
setValue : comparable -> Value -> Fields comparable -> Fields comparable
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
