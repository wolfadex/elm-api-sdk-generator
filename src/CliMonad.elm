module CliMonad exposing
    ( CliMonad
    , Message
    , NamespaceScope
    , Path
    , andThen
    , andThen2
    , combine
    , combineDict
    , combineMap
    , errorToWarning
    , fail
    , fromApiSpec
    , map
    , map2
    , map3
    , oneOfToVariantName
    , recordType
    , refToTypeName
    , run
    , stepOrFail
    , succeed
    , todo
    , todoWithDefault
    , typeToAnnotation
    , typeToAnnotationMaybe
    , withPath
    , withWarning
    )

import Common exposing (Field, Object, OneOfData, Type(..), TypeName, VariantName, toValueName)
import Dict
import Elm
import Elm.Annotation
import FastDict exposing (Dict)
import Gen.Bytes
import Gen.Debug
import Gen.Json.Encode
import Gen.Maybe
import OpenApi exposing (OpenApi)


type alias Message =
    { message : String
    , path : Path
    }


type alias Path =
    List String


type alias NamespaceScope =
    { within : List String
    , global : List String
    , source : List String
    }


type alias OneOfName =
    TypeName


type CliMonad a
    = CliMonad
        ({ openApi : OpenApi, generateTodos : Bool }
         ->
            Result
                Message
                ( a
                , List Message
                , Dict OneOfName OneOfData
                )
        )


withPath : String -> CliMonad a -> CliMonad a
withPath segment (CliMonad f) =
    CliMonad
        (\inputs ->
            case f inputs of
                Err message ->
                    Err (addPath segment message)

                Ok ( res, warns, enums ) ->
                    Ok ( res, List.map (addPath segment) warns, enums )
        )


addPath : String -> Message -> Message
addPath segment { path, message } =
    { path = segment :: path
    , message = message
    }


withWarning : String -> CliMonad a -> CliMonad a
withWarning message (CliMonad f) =
    CliMonad
        (\inputs ->
            Result.map
                (\( res, warnings, enums ) -> ( res, { path = [], message = message } :: warnings, enums ))
                (f inputs)
        )


todo : String -> CliMonad Elm.Expression
todo message =
    todoWithDefault (Gen.Debug.todo message) message


todoWithDefault : a -> String -> CliMonad a
todoWithDefault default message =
    CliMonad
        (\{ generateTodos } ->
            if generateTodos then
                Ok ( default, [ { path = [], message = message } ], FastDict.empty )

            else
                Err
                    { path = []
                    , message = "Todo: " ++ message
                    }
        )


fail : String -> CliMonad a
fail message =
    CliMonad (\_ -> Err { path = [], message = message })


succeed : a -> CliMonad a
succeed x =
    CliMonad (\_ -> Ok ( x, [], FastDict.empty ))


map : (a -> b) -> CliMonad a -> CliMonad b
map f (CliMonad x) =
    CliMonad (\input -> Result.map (\( xr, xw, xm ) -> ( f xr, xw, xm )) (x input))


map2 : (a -> b -> c) -> CliMonad a -> CliMonad b -> CliMonad c
map2 f (CliMonad x) (CliMonad y) =
    CliMonad (\input -> Result.map2 (\( xr, xw, xm ) ( yr, yw, ym ) -> ( f xr yr, xw ++ yw, FastDict.union xm ym )) (x input) (y input))


map3 : (a -> b -> c -> d) -> CliMonad a -> CliMonad b -> CliMonad c -> CliMonad d
map3 f (CliMonad x) (CliMonad y) (CliMonad z) =
    CliMonad (\input -> Result.map3 (\( xr, xw, xm ) ( yr, yw, ym ) ( zr, zw, zm ) -> ( f xr yr zr, xw ++ yw ++ zw, FastDict.union (FastDict.union xm ym) zm )) (x input) (y input) (z input))


andThen : (a -> CliMonad b) -> CliMonad a -> CliMonad b
andThen f (CliMonad x) =
    CliMonad
        (\input ->
            Result.andThen
                (\( y, yw, ym ) ->
                    let
                        (CliMonad z) =
                            f y
                    in
                    z input
                        |> Result.map (\( w, ww, wm ) -> ( w, yw ++ ww, FastDict.union ym wm ))
                )
                (x input)
        )


andThen2 : (a -> b -> CliMonad c) -> CliMonad a -> CliMonad b -> CliMonad c
andThen2 f x y =
    x
        |> andThen
            (\xr ->
                y
                    |> andThen (\yr -> f xr yr)
            )


{-| Runs the transformation from OpenApi to declaration.

Automatically appends the needed `enum` declarations.

-}
run : { openApi : OpenApi, generateTodos : Bool } -> CliMonad (List Elm.Declaration) -> Result Message ( List Elm.Declaration, List Message )
run input (CliMonad x) =
    x input
        |> Result.andThen
            (\( decls, warnings, oneOfs ) ->
                let
                    (CliMonad h) =
                        oneOfDeclarations oneOfs |> withPath "While generating `oneOf`s"
                in
                h input
                    |> Result.map
                        (\( enumDecls, enumWarnings, _ ) ->
                            ( decls ++ enumDecls
                            , (enumWarnings ++ warnings)
                                |> List.reverse
                            )
                        )
            )


oneOfDeclarations : Dict OneOfName OneOfData -> CliMonad (List Elm.Declaration)
oneOfDeclarations enums =
    combineMap
        oneOfDeclaration
        (FastDict.toList enums)


oneOfDeclaration : ( OneOfName, OneOfData ) -> CliMonad Elm.Declaration
oneOfDeclaration ( oneOfName, variants ) =
    let
        variantDeclaration : { name : VariantName, type_ : Type } -> CliMonad Elm.Variant
        variantDeclaration { name, type_ } =
            typeToAnnotation
                { global = []
                , source = []
                , within = []
                }
                type_
                |> map
                    (\variantAnnotation ->
                        let
                            variantName : VariantName
                            variantName =
                                oneOfToVariantName oneOfName name
                        in
                        Elm.variantWith variantName [ variantAnnotation ]
                    )
    in
    variants
        |> combineMap variantDeclaration
        |> map
            (Elm.customType oneOfName
                >> Elm.exposeWith
                    { exposeConstructor = True
                    , group = Just "Types"
                    }
            )


oneOfToVariantName : String -> String -> String
oneOfToVariantName prefix name =
    prefix ++ "__" ++ name


combineMap : (a -> CliMonad b) -> List a -> CliMonad (List b)
combineMap f ls =
    combine (List.map f ls)


combine : List (CliMonad a) -> CliMonad (List a)
combine =
    List.foldr (map2 (::)) (succeed [])


fromApiSpec : (OpenApi -> a) -> CliMonad a
fromApiSpec f =
    CliMonad (\input -> Ok ( f input.openApi, [], FastDict.empty ))


errorToWarning : CliMonad a -> CliMonad (Maybe a)
errorToWarning (CliMonad f) =
    CliMonad
        (\input ->
            case f input of
                Ok ( res, warns, enums ) ->
                    Ok ( Just res, warns, enums )

                Err { path, message } ->
                    Ok ( Nothing, [ { path = path, message = message } ], FastDict.empty )
        )


objectToAnnotation : { useMaybe : Bool, namespace : NamespaceScope } -> Object -> CliMonad Elm.Annotation.Annotation
objectToAnnotation config fields =
    FastDict.toList fields
        |> combineMap (\( k, v ) -> map (Tuple.pair (Common.toValueName k)) (fieldToAnnotation config v))
        |> map recordType


fieldToAnnotation : { useMaybe : Bool, namespace : NamespaceScope } -> Field -> CliMonad Elm.Annotation.Annotation
fieldToAnnotation { useMaybe, namespace } { type_, required } =
    let
        annotation : CliMonad Elm.Annotation.Annotation
        annotation =
            if useMaybe then
                typeToAnnotationMaybe namespace type_

            else
                typeToAnnotation namespace type_
    in
    if required then
        annotation

    else
        map Gen.Maybe.annotation_.maybe annotation


recordType : List ( String, Elm.Annotation.Annotation ) -> Elm.Annotation.Annotation
recordType fields =
    fields
        |> List.map (Tuple.mapFirst toValueName)
        |> Elm.Annotation.record


typeToAnnotation : NamespaceScope -> Type -> CliMonad Elm.Annotation.Annotation
typeToAnnotation namespace type_ =
    case type_ of
        Nullable t ->
            typeToAnnotation namespace t
                |> map
                    (\ann ->
                        Elm.Annotation.namedWith (namespace.global ++ [ "OpenApi" ])
                            "Nullable"
                            [ ann ]
                    )

        Object fields ->
            objectToAnnotation { useMaybe = False, namespace = namespace } fields

        String ->
            succeed Elm.Annotation.string

        Int ->
            succeed Elm.Annotation.int

        Float ->
            succeed Elm.Annotation.float

        Bool ->
            succeed Elm.Annotation.bool

        List t ->
            map Elm.Annotation.list (typeToAnnotation namespace t)

        OneOf oneOfName oneOfData ->
            oneOfAnnotation namespace oneOfName oneOfData

        Value ->
            succeed Gen.Json.Encode.annotation_.value

        Ref ref ->
            map
                (Elm.Annotation.named
                    (if namespace.within == namespace.source then
                        []

                     else
                        namespace.global ++ namespace.source
                    )
                )
                (refToTypeName ref)

        Bytes ->
            succeed Gen.Bytes.annotation_.bytes

        Unit ->
            succeed Elm.Annotation.unit


typeToAnnotationMaybe : NamespaceScope -> Type -> CliMonad Elm.Annotation.Annotation
typeToAnnotationMaybe namespace type_ =
    case type_ of
        Nullable t ->
            map Elm.Annotation.maybe (typeToAnnotationMaybe namespace t)

        Object fields ->
            objectToAnnotation { useMaybe = True, namespace = namespace } fields

        String ->
            succeed Elm.Annotation.string

        Int ->
            succeed Elm.Annotation.int

        Float ->
            succeed Elm.Annotation.float

        Bool ->
            succeed Elm.Annotation.bool

        List t ->
            map Elm.Annotation.list (typeToAnnotationMaybe namespace t)

        OneOf oneOfName oneOfData ->
            oneOfAnnotation namespace oneOfName oneOfData

        Value ->
            succeed Gen.Json.Encode.annotation_.value

        Ref ref ->
            map
                (Elm.Annotation.named
                    (if namespace.within == namespace.source then
                        []

                     else
                        namespace.global ++ namespace.source
                    )
                )
                (refToTypeName ref)

        Bytes ->
            succeed Gen.Bytes.annotation_.bytes

        Unit ->
            succeed Elm.Annotation.unit


oneOfAnnotation : NamespaceScope -> TypeName -> OneOfData -> CliMonad Elm.Annotation.Annotation
oneOfAnnotation namespace oneOfName oneOfData =
    CliMonad
        (\_ ->
            Ok
                ( Elm.Annotation.named
                    (if namespace.within == namespace.source then
                        []

                     else
                        namespace.global ++ namespace.source
                    )
                    oneOfName
                , []
                , FastDict.singleton oneOfName oneOfData
                )
        )


stepOrFail : String -> (a -> Maybe value) -> CliMonad a -> CliMonad value
stepOrFail msg f =
    andThen
        (\y ->
            case f y of
                Just z ->
                    succeed z

                Nothing ->
                    fail msg
        )


refToTypeName : List String -> CliMonad TypeName
refToTypeName ref =
    case ref of
        [ "#", "components", _, name ] ->
            succeed (Common.typifyName name)

        _ ->
            fail <| "Couldn't get the type ref (" ++ String.join "/" ref ++ ") for the response"


combineDict : Dict.Dict comparable (CliMonad a) -> CliMonad (Dict.Dict comparable a)
combineDict dict =
    dict
        |> Dict.foldr
            (\k vcm rescm ->
                map2
                    (\v res ->
                        ( k, v ) :: res
                    )
                    vcm
                    rescm
            )
            (succeed [])
        |> map Dict.fromList
