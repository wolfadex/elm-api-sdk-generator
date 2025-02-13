module SchemaUtils exposing
    ( OneOfName
    , decodeOptionalField
    , decodeOptionalFieldDocumentation
    , getAlias
    , oneOfDeclarations
    , recordType
    , refToTypeName
    , schemaToType
    , toVariantName
    , typeToAnnotationWithMaybe
    , typeToAnnotationWithNullable
    , typeToDecoder
    , typeToEncoder
    )

import CliMonad exposing (CliMonad)
import Common
import Dict
import Elm
import Elm.Annotation
import Elm.Arg
import Elm.Case
import Elm.Declare
import Elm.Let
import Elm.Op
import Elm.ToString
import FastDict
import Gen.Basics
import Gen.Bytes
import Gen.Dict
import Gen.Json.Decode
import Gen.Json.Encode
import Gen.List
import Gen.Maybe
import Gen.OpenApi.Common
import Gen.Result
import Gen.String
import Json.Decode
import Json.Encode
import Json.Schema.Definitions
import Maybe.Extra
import OpenApi
import OpenApi.Components
import OpenApi.Schema
import Result.Extra
import Set exposing (Set)


getSchema : String -> CliMonad Json.Schema.Definitions.Schema
getSchema refName =
    CliMonad.fromApiSpec identity
        |> CliMonad.stepOrFail ("Could not find components in the schema, while looking up " ++ refName)
            OpenApi.components
        |> CliMonad.stepOrFail ("Could not find component's schema, while looking up " ++ refName)
            (\components -> Dict.get refName (OpenApi.Components.schemas components))
        |> CliMonad.map OpenApi.Schema.get


getAlias : List String -> CliMonad Json.Schema.Definitions.Schema
getAlias refUri =
    case refUri of
        [ "#", "components", _, name ] ->
            getSchema name

        _ ->
            CliMonad.fail <| "Couldn't get the type ref (" ++ String.join "/" refUri ++ ") for the response"


subSchemaAllOfToProperties : Bool -> Json.Schema.Definitions.SubSchema -> CliMonad (List ( Common.UnsafeName, Common.Field ))
subSchemaAllOfToProperties qualify subSchema =
    subSchema.allOf
        |> Maybe.withDefault []
        |> CliMonad.combineMap (schemaToProperties qualify)
        |> CliMonad.map (List.foldl listUnion [])


schemaToProperties : Bool -> Json.Schema.Definitions.Schema -> CliMonad (List ( Common.UnsafeName, Common.Field ))
schemaToProperties qualify allOfItem =
    case allOfItem of
        Json.Schema.Definitions.ObjectSchema allOfItemSchema ->
            CliMonad.map3
                (\a b c ->
                    listUnion a b
                        |> listUnion c
                )
                (subSchemaToProperties qualify allOfItemSchema)
                (subSchemaRefToProperties qualify allOfItemSchema)
                (subSchemaAllOfToProperties qualify allOfItemSchema)

        Json.Schema.Definitions.BooleanSchema _ ->
            CliMonad.todoWithDefault [] "Boolean schema inside allOf"


listUnion : List ( Common.UnsafeName, a ) -> List ( Common.UnsafeName, a ) -> List ( Common.UnsafeName, a )
listUnion l r =
    let
        toDict : List ( Common.UnsafeName, b ) -> FastDict.Dict String b
        toDict v =
            FastDict.fromList (List.map (Tuple.mapFirst Common.unwrapUnsafe) v)
    in
    FastDict.union (toDict l) (toDict r)
        |> FastDict.toList
        |> List.map (Tuple.mapFirst Common.UnsafeName)


subSchemaRefToProperties : Bool -> Json.Schema.Definitions.SubSchema -> CliMonad (List ( Common.UnsafeName, Common.Field ))
subSchemaRefToProperties qualify allOfItem =
    case allOfItem.ref of
        Nothing ->
            CliMonad.succeed []

        Just ref ->
            getAlias (String.split "/" ref)
                |> CliMonad.withPath (Common.UnsafeName ref)
                |> CliMonad.andThen (schemaToProperties qualify)


subSchemaToProperties : Bool -> Json.Schema.Definitions.SubSchema -> CliMonad (List ( Common.UnsafeName, Common.Field ))
subSchemaToProperties qualify sch =
    -- TODO: rename
    let
        requiredSet : Set String
        requiredSet =
            sch.required
                |> Maybe.withDefault []
                |> Set.fromList
    in
    sch.properties
        |> Maybe.map (\(Json.Schema.Definitions.Schemata schemata) -> schemata)
        |> Maybe.withDefault []
        |> CliMonad.combineMap
            (\( key, valueSchema ) ->
                schemaToType qualify valueSchema
                    |> CliMonad.withPath (Common.UnsafeName key)
                    |> CliMonad.map
                        (\{ type_, documentation } ->
                            ( Common.UnsafeName key
                            , { type_ = type_
                              , required = Set.member key requiredSet
                              , documentation = documentation
                              }
                            )
                        )
            )


schemaToType : Bool -> Json.Schema.Definitions.Schema -> CliMonad { type_ : Common.Type, documentation : Maybe String }
schemaToType qualify schema =
    case schema of
        Json.Schema.Definitions.BooleanSchema _ ->
            CliMonad.todoWithDefault { type_ = Common.Value, documentation = Nothing } "Boolean schema"

        Json.Schema.Definitions.ObjectSchema subSchema ->
            let
                nullable : CliMonad { type_ : Common.Type, documentation : Maybe String } -> CliMonad { type_ : Common.Type, documentation : Maybe String }
                nullable =
                    CliMonad.map
                        (\{ type_, documentation } ->
                            { type_ = Common.Nullable type_
                            , documentation = documentation
                            }
                        )

                singleTypeToType : Json.Schema.Definitions.SingleType -> CliMonad { type_ : Common.Type, documentation : Maybe String }
                singleTypeToType singleType =
                    let
                        basic : Common.BasicType -> (a -> Common.ConstValue) -> Json.Decode.Decoder a -> CliMonad { type_ : Common.Type, documentation : Maybe String }
                        basic basicType toConst decoder =
                            let
                                build : Maybe a -> CliMonad { type_ : Common.Type, documentation : Maybe String }
                                build const =
                                    { type_ =
                                        Common.Basic basicType
                                            { const = Maybe.map toConst const
                                            , format = subSchema.format
                                            }
                                    , documentation = subSchema.description
                                    }
                                        |> CliMonad.succeed
                            in
                            case subSchema.const of
                                Nothing ->
                                    build Nothing

                                Just const ->
                                    case Json.Decode.decodeValue decoder const of
                                        Ok value ->
                                            build (Just value)

                                        Err _ ->
                                            CliMonad.fail ("Invalid const value: " ++ Json.Encode.encode 0 const)
                    in
                    case singleType of
                        Json.Schema.Definitions.ObjectType ->
                            objectSchemaToType qualify subSchema

                        Json.Schema.Definitions.StringType ->
                            basic Common.String Common.ConstString Json.Decode.string

                        Json.Schema.Definitions.IntegerType ->
                            basic Common.Integer Common.ConstInteger Json.Decode.int

                        Json.Schema.Definitions.NumberType ->
                            basic Common.Number Common.ConstNumber Json.Decode.float

                        Json.Schema.Definitions.BooleanType ->
                            basic Common.Boolean Common.ConstBoolean Json.Decode.bool

                        Json.Schema.Definitions.NullType ->
                            CliMonad.succeed { type_ = Common.Null, documentation = subSchema.description }

                        Json.Schema.Definitions.ArrayType ->
                            case subSchema.items of
                                Json.Schema.Definitions.NoItems ->
                                    CliMonad.fail "Found an array type but no items definition"

                                Json.Schema.Definitions.ArrayOfItems _ ->
                                    CliMonad.todoWithDefault { type_ = Common.Value, documentation = subSchema.description } "Array of items as item definition"

                                Json.Schema.Definitions.ItemDefinition itemSchema ->
                                    CliMonad.map
                                        (\{ type_, documentation } ->
                                            { type_ = Common.List type_
                                            , documentation =
                                                [ subSchema.description
                                                , Maybe.map
                                                    (\doc ->
                                                        if String.contains "\n" doc then
                                                            "A list of:\n" ++ doc

                                                        else
                                                            "A list of: " ++ doc
                                                    )
                                                    documentation
                                                ]
                                                    |> joinIfNotEmpty "\n\n"
                                            }
                                        )
                                        (schemaToType qualify itemSchema)

                anyOfToType : List Json.Schema.Definitions.Schema -> CliMonad { type_ : Common.Type, documentation : Maybe String }
                anyOfToType _ =
                    CliMonad.succeed { type_ = Common.Value, documentation = subSchema.description }

                oneOfCombine : List Json.Schema.Definitions.Schema -> CliMonad { type_ : Common.Type, documentation : Maybe String }
                oneOfCombine oneOf =
                    CliMonad.combineMap (schemaToType qualify) oneOf
                        |> CliMonad.andThen oneOfType
                        |> CliMonad.map
                            (\{ type_, documentation } ->
                                { type_ = type_
                                , documentation =
                                    [ subSchema.description
                                    , documentation
                                    ]
                                        |> joinIfNotEmpty "\n\n"
                                }
                            )

                oneOfToType : List Json.Schema.Definitions.Schema -> CliMonad { type_ : Common.Type, documentation : Maybe String }
                oneOfToType oneOf =
                    case oneOf of
                        -- handle the oneOf syntax for nullable values
                        -- "oneOf": [
                        --   { "type": "null" },
                        --   { "$ref": "#/components/schemas/Foo" }
                        -- ]
                        [ (Json.Schema.Definitions.ObjectSchema firstSchema) as first, (Json.Schema.Definitions.ObjectSchema secondSchema) as second ] ->
                            case ( firstSchema.type_, secondSchema.type_ ) of
                                ( Json.Schema.Definitions.SingleType Json.Schema.Definitions.NullType, _ ) ->
                                    schemaToType qualify second
                                        |> nullable

                                ( _, Json.Schema.Definitions.SingleType Json.Schema.Definitions.NullType ) ->
                                    schemaToType qualify first
                                        |> nullable

                                _ ->
                                    oneOfCombine oneOf

                        _ ->
                            oneOfCombine oneOf
            in
            case subSchema.type_ of
                Json.Schema.Definitions.SingleType Json.Schema.Definitions.StringType ->
                    case subSchema.enum of
                        Nothing ->
                            singleTypeToType Json.Schema.Definitions.StringType

                        Just enums ->
                            case Result.Extra.combineMap (Json.Decode.decodeValue Json.Decode.string) enums of
                                Err _ ->
                                    CliMonad.fail "Attempted to parse an enum as a string and failed"

                                Ok decodedEnums ->
                                    CliMonad.succeed
                                        { type_ = Common.enum decodedEnums
                                        , documentation = subSchema.description
                                        }

                Json.Schema.Definitions.SingleType singleType ->
                    singleTypeToType singleType

                Json.Schema.Definitions.AnyType ->
                    case subSchema.ref of
                        Just ref ->
                            CliMonad.succeed { type_ = Common.ref ref, documentation = subSchema.description }

                        Nothing ->
                            case subSchema.anyOf of
                                Just [ onlySchema ] ->
                                    schemaToType qualify onlySchema

                                Just [ firstSchema, secondSchema ] ->
                                    case ( firstSchema, secondSchema ) of
                                        ( Json.Schema.Definitions.ObjectSchema firstSubSchema, Json.Schema.Definitions.ObjectSchema secondSubSchema ) ->
                                            -- The first 2 cases here are for pseudo-nullable schemas where the higher level schema type is AnyOf
                                            -- but it's actually made up of only 2 types and 1 of them is nullable. This acts as a hack of sorts to
                                            -- mark a value as nullable in the schema.
                                            case ( firstSubSchema.type_, secondSubSchema.type_ ) of
                                                ( Json.Schema.Definitions.SingleType Json.Schema.Definitions.NullType, _ ) ->
                                                    nullable (schemaToType qualify secondSchema)

                                                ( _, Json.Schema.Definitions.SingleType Json.Schema.Definitions.NullType ) ->
                                                    nullable (schemaToType qualify firstSchema)

                                                _ ->
                                                    anyOfToType [ firstSchema, secondSchema ]

                                        _ ->
                                            anyOfToType [ firstSchema, secondSchema ]

                                Just anyOf ->
                                    anyOfToType anyOf

                                Nothing ->
                                    case subSchema.allOf of
                                        Just [ onlySchema ] ->
                                            schemaToType qualify onlySchema

                                        Just [] ->
                                            CliMonad.succeed { type_ = Common.Value, documentation = subSchema.description }

                                        Just _ ->
                                            -- If we have more than one item in `allOf`, then it's _probably_ an object
                                            -- TODO: improve this to actually check if all the `allOf` subschema are objects.
                                            objectSchemaToType qualify subSchema

                                        Nothing ->
                                            case subSchema.oneOf of
                                                Just [ onlySchema ] ->
                                                    schemaToType qualify onlySchema

                                                Just [] ->
                                                    CliMonad.succeed { type_ = Common.Value, documentation = subSchema.description }

                                                Just oneOfs ->
                                                    oneOfToType oneOfs

                                                Nothing ->
                                                    case subSchema.enum of
                                                        Nothing ->
                                                            CliMonad.succeed { type_ = Common.Value, documentation = subSchema.description }

                                                        Just enums ->
                                                            case Result.Extra.combineMap (Json.Decode.decodeValue Json.Decode.string) enums of
                                                                Err _ ->
                                                                    CliMonad.fail "Attempted to parse an enum as a string and failed"

                                                                Ok decodedEnums ->
                                                                    CliMonad.succeed
                                                                        { type_ = Common.enum decodedEnums
                                                                        , documentation = subSchema.description
                                                                        }

                Json.Schema.Definitions.NullableType singleType ->
                    nullable (singleTypeToType singleType)

                Json.Schema.Definitions.UnionType singleTypes ->
                    let
                        ( nulls, nonNulls ) =
                            List.partition
                                (\st -> st == Json.Schema.Definitions.NullType)
                                singleTypes
                    in
                    nonNulls
                        |> CliMonad.combineMap singleTypeToType
                        |> CliMonad.andThen oneOfType
                        |> CliMonad.map
                            (\{ type_, documentation } ->
                                { type_ =
                                    if List.isEmpty nulls then
                                        type_

                                    else
                                        Common.Nullable type_
                                , documentation = documentation
                                }
                            )


typeToOneOfVariant :
    Bool
    -> { type_ : Common.Type, documentation : Maybe String }
    -> CliMonad (Maybe { name : Common.UnsafeName, type_ : Common.Type, documentation : Maybe String })
typeToOneOfVariant qualify { type_, documentation } =
    type_
        |> typeToAnnotationWithNullable qualify
        |> CliMonad.map
            (\ann ->
                let
                    rawName : String
                    rawName =
                        ann
                            |> Elm.ToString.annotation
                            |> .signature
                in
                if String.contains "{" rawName then
                    Nothing

                else
                    Just
                        { name = Common.UnsafeName rawName
                        , type_ = type_
                        , documentation = documentation
                        }
            )


oneOfType :
    List { type_ : Common.Type, documentation : Maybe String }
    -> CliMonad { type_ : Common.Type, documentation : Maybe String }
oneOfType types =
    types
        |> CliMonad.combineMap (typeToOneOfVariant False)
        |> CliMonad.map
            (\maybeVariants ->
                case Maybe.Extra.combine maybeVariants of
                    Nothing ->
                        { type_ = Common.Value, documentation = Nothing }

                    Just variants ->
                        let
                            sortedVariants :
                                List
                                    { name : Common.UnsafeName
                                    , type_ : Common.Type
                                    , documentation : Maybe String
                                    }
                            sortedVariants =
                                List.sortBy (\{ name } -> Common.unwrapUnsafe name) variants

                            names : List Common.UnsafeName
                            names =
                                List.map .name sortedVariants
                        in
                        { type_ =
                            Common.OneOf
                                (names
                                    |> List.map fixOneOfName
                                    |> String.join "_Or_"
                                )
                                sortedVariants
                        , documentation =
                            sortedVariants
                                |> List.map (\{ name, documentation } -> Maybe.map (\doc -> " - " ++ Common.toValueName name ++ ": " ++ doc) documentation)
                                |> joinIfNotEmpty "\n\n"
                                |> Maybe.map (\doc -> "This is a oneOf. The alternatives are:\n\n" ++ doc)
                        }
            )


{-| Transform an object schema's named and inherited (via $ref) properties to a type
-}
objectSchemaToTypeHelp : Bool -> Json.Schema.Definitions.SubSchema -> CliMonad { type_ : Common.Type, documentation : Maybe String }
objectSchemaToTypeHelp qualify subSchema =
    CliMonad.map2
        (\schemaProps allOfProps ->
            let
                ( props, propsDocumentations ) =
                    listUnion schemaProps allOfProps
                        |> List.map
                            (\( name, field ) ->
                                ( ( name, field )
                                , field.documentation
                                    |> Maybe.map
                                        (\doc ->
                                            " - " ++ Common.toValueName name ++ ": " ++ String.join "\n      " (String.split "\n" doc)
                                        )
                                )
                            )
                        |> List.unzip

                propsDocumentation : Maybe String
                propsDocumentation =
                    propsDocumentations
                        |> joinIfNotEmpty "\n"
            in
            { type_ = Common.Object props
            , documentation =
                case propsDocumentation of
                    Nothing ->
                        subSchema.description

                    Just _ ->
                        [ Just (Maybe.withDefault "Fields:" subSchema.description) -- A nonempty line is needed for formatting
                        , propsDocumentation
                        ]
                            |> joinIfNotEmpty "\n\n"
            }
        )
        (subSchemaToProperties qualify subSchema)
        (subSchemaAllOfToProperties qualify subSchema)


objectSchemaToType : Bool -> Json.Schema.Definitions.SubSchema -> CliMonad { type_ : Common.Type, documentation : Maybe String }
objectSchemaToType qualify subSchema =
    let
        declaredProperties : CliMonad { type_ : Common.Type, documentation : Maybe String }
        declaredProperties =
            objectSchemaToTypeHelp qualify subSchema

        declaredAndAdditionalProperties : CliMonad { type_ : Common.Type, documentation : Maybe String } -> CliMonad { type_ : Common.Type, documentation : Maybe String }
        declaredAndAdditionalProperties additionalSchema =
            CliMonad.andThen2
                (\declaredProperties_ additionalProperties ->
                    case declaredProperties_.type_ of
                        Common.Object properties ->
                            { type_ = Common.Dict additionalProperties properties
                            , documentation =
                                [ declaredProperties_.documentation
                                , additionalProperties.documentation
                                    |> Maybe.map
                                        (\doc ->
                                            case String.lines doc of
                                                [] ->
                                                    " - additionalProperties"

                                                first :: [] ->
                                                    " - additionalProperties: " ++ first

                                                first :: rest ->
                                                    " - additionalProperties: "
                                                        ++ first
                                                        ++ "\n\n   Each value in the dict is a record of:    "
                                                        ++ String.join "\n    " rest
                                        )
                                ]
                                    |> joinIfNotEmpty "\n\n"
                            }
                                |> CliMonad.succeed

                        _ ->
                            CliMonad.fail "Internal error: non-Object from objectSchemaToTypeHelp"
                )
                declaredProperties
                additionalSchema
    in
    case subSchema.additionalProperties of
        -- Object contains only specified properties, not arbitrary extra ones.
        Just (Json.Schema.Definitions.BooleanSchema False) ->
            declaredProperties

        -- It's unclear whether "true" is *technically* an acceptable value for
        -- additionalProperties, but the GitHub API spec uses it. Since the type
        -- of the properties could be anything, we keep them as Json Values.
        Just (Json.Schema.Definitions.BooleanSchema True) ->
            CliMonad.succeed
                { type_ = Common.Value
                , documentation = Just "Arbitrary data whose type is not defined by the API spec"
                }
                |> declaredAndAdditionalProperties

        -- The object contains an additionalProperties entry that describes the
        -- type of the values, which may have arbitrary keys, in the object.
        Just additionalPropertiesSchema ->
            schemaToType qualify additionalPropertiesSchema
                |> declaredAndAdditionalProperties

        Nothing ->
            declaredProperties


joinIfNotEmpty : String -> List (Maybe String) -> Maybe String
joinIfNotEmpty sep chunks =
    let
        filtered : List String
        filtered =
            List.filterMap identity chunks
    in
    if List.isEmpty filtered then
        Nothing

    else
        Just (String.join sep filtered)


type alias OneOfName =
    Common.TypeName


oneOfDeclarations :
    FastDict.Dict OneOfName Common.OneOfData
    -> CliMonad (List CliMonad.Declaration)
oneOfDeclarations enums =
    CliMonad.combineMap
        oneOfDeclaration
        (FastDict.toList enums)


oneOfDeclaration :
    ( OneOfName, Common.OneOfData )
    -> CliMonad CliMonad.Declaration
oneOfDeclaration ( oneOfName, variants ) =
    let
        variantDeclaration : { name : Common.UnsafeName, type_ : Common.Type, documentation : Maybe String } -> CliMonad Elm.Variant
        variantDeclaration { name, type_ } =
            typeToAnnotationWithNullable False type_
                |> CliMonad.map
                    (\variantAnnotation ->
                        let
                            variantName : Common.VariantName
                            variantName =
                                toVariantName oneOfName name
                        in
                        Elm.variantWith variantName [ variantAnnotation ]
                    )
    in
    variants
        |> CliMonad.combineMap variantDeclaration
        |> CliMonad.map
            (\decl ->
                { moduleName = Common.Types
                , name = oneOfName
                , group = "One of"
                , declaration =
                    decl
                        |> Elm.customType oneOfName
                        |> Elm.exposeConstructor
                }
            )


toVariantName : Common.TypeName -> Common.UnsafeName -> String
toVariantName oneOfName variantName =
    oneOfName ++ "__" ++ fixOneOfName variantName


{-| When we go from `Elm.Annotation` to `String` it includes the module name if it's an imported type.
We don't want that for our generated types, so we remove it here.
-}
fixOneOfName : Common.UnsafeName -> String
fixOneOfName name =
    name
        |> Common.unwrapUnsafe
        |> String.replace "OpenApi.Nullable" "Nullable"
        |> String.replace "." ""
        |> Common.UnsafeName
        |> Common.toTypeName


{-| Transform an OpenAPI type into an Elm annotation. Nullable values are represented using Nullable.
-}
typeToAnnotationWithNullable : Bool -> Common.Type -> CliMonad Elm.Annotation.Annotation
typeToAnnotationWithNullable qualify type_ =
    case type_ of
        Common.Nullable t ->
            CliMonad.map
                Gen.OpenApi.Common.annotation_.nullable
                (typeToAnnotationWithNullable qualify t)

        Common.Object fields ->
            objectToAnnotation qualify { useMaybe = False } fields

        Common.Basic basicType basic ->
            basicTypeToAnnotation basicType basic

        Common.Null ->
            CliMonad.succeed Elm.Annotation.unit

        Common.List t ->
            CliMonad.map Elm.Annotation.list (typeToAnnotationWithNullable qualify t)

        Common.Dict additionalProperties [] ->
            -- We do not use `Elm.Annotation.dict` here because it will NOT
            -- result in `import Dict` being generated in the module, due to
            -- a bug in elm-codegen.
            CliMonad.map (Gen.Dict.annotation_.dict Elm.Annotation.string)
                (typeToAnnotationWithNullable qualify additionalProperties.type_)

        Common.Dict additionalProperties fields ->
            let
                additionalPropertiesField :
                    ( Common.UnsafeName
                    , { type_ : Common.Type, required : Bool, documentation : Maybe String }
                    )
                additionalPropertiesField =
                    ( Common.UnsafeName "additionalProperties"
                    , { type_ = Common.Dict additionalProperties []
                      , required = True
                      , documentation = additionalProperties.documentation
                      }
                    )
            in
            (additionalPropertiesField :: fields)
                |> objectToAnnotation qualify { useMaybe = False }

        Common.Enum variants ->
            CliMonad.enumName variants
                |> CliMonad.andThen
                    (\maybeName ->
                        case maybeName of
                            Nothing ->
                                CliMonad.succeed Elm.Annotation.string

                            Just name ->
                                CliMonad.map
                                    (\typesNamespace ->
                                        if qualify then
                                            Elm.Annotation.named typesNamespace (Common.toTypeName name)

                                        else
                                            Elm.Annotation.named [] (Common.toTypeName name)
                                    )
                                    (CliMonad.moduleToNamespace Common.Types)
                    )

        Common.OneOf oneOfName oneOfData ->
            oneOfAnnotation qualify oneOfName oneOfData

        Common.Value ->
            CliMonad.succeed Gen.Json.Encode.annotation_.value

        Common.Ref ref ->
            CliMonad.map2
                (\typesNamespace name ->
                    if qualify then
                        Elm.Annotation.named typesNamespace (Common.toTypeName name)

                    else
                        Elm.Annotation.named [] (Common.toTypeName name)
                )
                (CliMonad.moduleToNamespace Common.Types)
                (refToTypeName ref)

        Common.Bytes ->
            CliMonad.succeed Gen.Bytes.annotation_.bytes
                |> CliMonad.withRequiredPackage "elm/bytes"

        Common.Unit ->
            CliMonad.succeed Elm.Annotation.unit


{-| Transform an OpenAPI type into an Elm annotation. Nullable values are represented using Maybe.
-}
typeToAnnotationWithMaybe : Bool -> Common.Type -> CliMonad Elm.Annotation.Annotation
typeToAnnotationWithMaybe qualify type_ =
    case type_ of
        Common.Nullable t ->
            CliMonad.map Elm.Annotation.maybe (typeToAnnotationWithMaybe qualify t)

        Common.Object fields ->
            objectToAnnotation qualify { useMaybe = True } fields

        Common.Basic basicType basic ->
            basicTypeToAnnotation basicType basic

        Common.Null ->
            CliMonad.succeed Elm.Annotation.unit

        Common.List t ->
            CliMonad.map Elm.Annotation.list (typeToAnnotationWithMaybe qualify t)

        Common.Dict additionalProperties [] ->
            -- We do not use `Elm.Annotation.dict` here because it will NOT
            -- result in `import Dict` being generated in the module, due to
            -- a bug in elm-codegen.
            CliMonad.map (Gen.Dict.annotation_.dict Elm.Annotation.string)
                (typeToAnnotationWithMaybe qualify additionalProperties.type_)

        Common.Dict additionalProperties fields ->
            let
                additionalPropertiesField :
                    ( Common.UnsafeName
                    , { type_ : Common.Type, required : Bool, documentation : Maybe String }
                    )
                additionalPropertiesField =
                    ( Common.UnsafeName "additionalProperties"
                    , { type_ = Common.Dict additionalProperties []
                      , required = True
                      , documentation = additionalProperties.documentation
                      }
                    )
            in
            (additionalPropertiesField :: fields)
                |> objectToAnnotation qualify { useMaybe = True }

        Common.Enum variants ->
            CliMonad.enumName variants
                |> CliMonad.andThen
                    (\maybeName ->
                        case maybeName of
                            Nothing ->
                                CliMonad.succeed Elm.Annotation.string

                            Just name ->
                                CliMonad.map
                                    (\typesNamespace ->
                                        if qualify then
                                            Elm.Annotation.named typesNamespace (Common.toTypeName name)

                                        else
                                            Elm.Annotation.named [] (Common.toTypeName name)
                                    )
                                    (CliMonad.moduleToNamespace Common.Types)
                    )

        Common.OneOf oneOfName oneOfData ->
            oneOfAnnotation qualify oneOfName oneOfData

        Common.Value ->
            CliMonad.succeed Gen.Json.Encode.annotation_.value

        Common.Ref ref ->
            CliMonad.map2
                (\typesNamespace name ->
                    if qualify then
                        Elm.Annotation.named typesNamespace (Common.toTypeName name)

                    else
                        Elm.Annotation.named [] (Common.toTypeName name)
                )
                (CliMonad.moduleToNamespace Common.Types)
                (refToTypeName ref)

        Common.Bytes ->
            CliMonad.succeed Gen.Bytes.annotation_.bytes
                |> CliMonad.withRequiredPackage "elm/bytes"

        Common.Unit ->
            CliMonad.succeed Elm.Annotation.unit


basicTypeToAnnotation : Common.BasicType -> { a | format : Maybe String } -> CliMonad Elm.Annotation.Annotation
basicTypeToAnnotation basicType { format } =
    let
        default : Elm.Annotation.Annotation
        default =
            case basicType of
                Common.String ->
                    Elm.Annotation.string

                Common.Integer ->
                    Elm.Annotation.int

                Common.Boolean ->
                    Elm.Annotation.bool

                Common.Number ->
                    Elm.Annotation.float
    in
    CliMonad.withFormat basicType format .annotation default


objectToAnnotation : Bool -> { useMaybe : Bool } -> Common.Object -> CliMonad Elm.Annotation.Annotation
objectToAnnotation qualify config fields =
    fields
        |> CliMonad.combineMap (\( k, v ) -> CliMonad.map (Tuple.pair k) (fieldToAnnotation qualify config v))
        |> CliMonad.map recordType


recordType : List ( Common.UnsafeName, Elm.Annotation.Annotation ) -> Elm.Annotation.Annotation
recordType fields =
    fields
        |> List.map (Tuple.mapFirst Common.toValueName)
        |> Elm.Annotation.record


fieldToAnnotation : Bool -> { useMaybe : Bool } -> Common.Field -> CliMonad Elm.Annotation.Annotation
fieldToAnnotation qualify { useMaybe } { type_, required } =
    let
        annotation : CliMonad Elm.Annotation.Annotation
        annotation =
            if useMaybe then
                typeToAnnotationWithMaybe qualify type_

            else
                typeToAnnotationWithNullable qualify type_
    in
    if required then
        annotation

    else
        CliMonad.map Gen.Maybe.annotation_.maybe annotation


typeToEncoder : Bool -> Common.Type -> CliMonad (Elm.Expression -> Elm.Expression)
typeToEncoder qualify type_ =
    case type_ of
        Common.Basic basicType basic ->
            basicTypeToEncoder basicType basic

        Common.Null ->
            CliMonad.succeed (\_ -> Gen.Json.Encode.null)

        Common.Enum variants ->
            CliMonad.enumName variants
                |> CliMonad.andThen
                    (\maybeName ->
                        case maybeName of
                            Nothing ->
                                CliMonad.succeed Gen.Json.Encode.call_.string

                            Just name ->
                                CliMonad.map
                                    (\jsonNamespace rec ->
                                        Elm.apply
                                            (Elm.value
                                                { importFrom =
                                                    if qualify then
                                                        jsonNamespace

                                                    else
                                                        []
                                                , name = "encode" ++ Common.toTypeName name
                                                , annotation = Nothing
                                                }
                                            )
                                            [ rec ]
                                    )
                                    (CliMonad.moduleToNamespace Common.Json)
                    )

        Common.Object properties ->
            let
                allRequired : Bool
                allRequired =
                    List.all (\( _, { required } ) -> required) properties
            in
            properties
                |> CliMonad.combineMap
                    (\( key, field ) ->
                        typeToEncoder qualify field.type_
                            |> CliMonad.map
                                (\encoder rec ->
                                    let
                                        fieldExpr : Elm.Expression
                                        fieldExpr =
                                            Elm.get (Common.toValueName key) rec

                                        toTuple : Elm.Expression -> Elm.Expression
                                        toTuple value =
                                            Elm.tuple
                                                (Elm.string (Common.unwrapUnsafe key))
                                                (encoder value)
                                    in
                                    if allRequired then
                                        toTuple fieldExpr

                                    else if field.required then
                                        Gen.Maybe.make_.just (toTuple fieldExpr)

                                    else
                                        Gen.Maybe.map toTuple fieldExpr
                                )
                    )
                |> CliMonad.map
                    (\toProperties value ->
                        if allRequired then
                            Gen.Json.Encode.object <|
                                List.map (\prop -> prop value) toProperties

                        else
                            Gen.Json.Encode.call_.object <|
                                Gen.List.filterMap Gen.Basics.identity <|
                                    List.map (\prop -> prop value) toProperties
                    )

        Common.List t ->
            typeToEncoder qualify t
                |> CliMonad.map
                    (\encoder ->
                        Gen.Json.Encode.call_.list (Elm.functionReduced "rec" encoder)
                    )

        -- An object with additionalProperties (the type of those properties is
        -- `additionalProperties`) and no normal `properties` fields: A simple Dict.
        Common.Dict additionalProperties [] ->
            typeToEncoder qualify additionalProperties.type_
                |> CliMonad.map
                    (\encoder ->
                        Gen.Json.Encode.call_.dict Gen.Basics.values_.identity (Elm.functionReduced "rec" encoder)
                    )

        -- An object with additionalProperties *and* normal `properties` fields.
        Common.Dict additionalProperties properties ->
            let
                allRequired : Bool
                allRequired =
                    List.all (\( _, { required } ) -> required) properties
            in
            properties
                |> CliMonad.combineMap
                    (\( key, field ) ->
                        typeToEncoder qualify field.type_
                            |> CliMonad.map
                                (\encoder rec ->
                                    let
                                        fieldExpr : Elm.Expression
                                        fieldExpr =
                                            Elm.get (Common.toValueName key) rec

                                        toTuple : Elm.Expression -> Elm.Expression
                                        toTuple value =
                                            Elm.tuple
                                                (Elm.string (Common.unwrapUnsafe key))
                                                (encoder value)
                                    in
                                    if allRequired then
                                        toTuple fieldExpr

                                    else if field.required then
                                        Gen.Maybe.make_.just (toTuple fieldExpr)

                                    else
                                        Gen.Maybe.map toTuple fieldExpr
                                )
                    )
                |> CliMonad.map2
                    (\additionalPropertyEncoder toProperties ->
                        \value ->
                            Gen.Json.Encode.call_.object
                                (Gen.List.call_.append
                                    (if allRequired then
                                        Elm.list (List.map (\prop -> prop value) toProperties)

                                     else
                                        Gen.List.filterMap Gen.Basics.identity (List.map (\prop -> prop value) toProperties)
                                    )
                                    (Gen.List.call_.map
                                        (Elm.fn (Elm.Arg.tuple (Elm.Arg.var "key") (Elm.Arg.var "value"))
                                            (\( key, data ) ->
                                                Elm.tuple key (additionalPropertyEncoder data)
                                            )
                                        )
                                        (Gen.Dict.toList (Elm.get "additionalProperties" value))
                                    )
                                )
                    )
                    (typeToEncoder qualify additionalProperties.type_)

        Common.Nullable t ->
            CliMonad.map
                (\encoder nullableValue ->
                    Elm.Case.custom
                        nullableValue
                        (Gen.OpenApi.Common.annotation_.nullable (Elm.Annotation.var "value"))
                        [ Elm.Case.branch (Elm.Arg.customType "Null" ()) (\_ -> Gen.Json.Encode.null)
                        , Elm.Case.branch
                            (Elm.Arg.customType "Present" identity
                                |> Elm.Arg.item (Elm.Arg.varWith "value" (Elm.Annotation.var "value"))
                            )
                            encoder
                        ]
                )
                (typeToEncoder qualify t)

        Common.Value ->
            CliMonad.succeed <| Gen.Basics.identity

        Common.Ref ref ->
            CliMonad.map2
                (\jsonNamespace name rec ->
                    Elm.apply
                        (Elm.value
                            { importFrom =
                                if qualify then
                                    jsonNamespace

                                else
                                    []
                            , name = "encode" ++ Common.toTypeName name
                            , annotation = Nothing
                            }
                        )
                        [ rec ]
                )
                (CliMonad.moduleToNamespace Common.Json)
                (refToTypeName ref)

        Common.OneOf oneOfName oneOfData ->
            oneOfData
                |> CliMonad.combineMap
                    (\variant ->
                        CliMonad.map2
                            (\ann variantEncoder ->
                                Elm.Case.branch
                                    (Elm.Arg.customType (toVariantName oneOfName variant.name) identity
                                        |> Elm.Arg.item (Elm.Arg.varWith "content" ann)
                                    )
                                    variantEncoder
                            )
                            (typeToAnnotationWithNullable True variant.type_)
                            (typeToEncoder qualify variant.type_)
                    )
                |> CliMonad.map2
                    (\typesNamespace branches rec ->
                        Elm.Case.custom rec
                            (Elm.Annotation.named typesNamespace oneOfName)
                            branches
                    )
                    (CliMonad.moduleToNamespace Common.Types)

        Common.Bytes ->
            CliMonad.todo "Encoder for bytes not implemented"
                |> CliMonad.map (\encoder _ -> encoder)

        Common.Unit ->
            CliMonad.succeed (\_ -> Gen.Json.Encode.null)


basicTypeToEncoder : Common.BasicType -> { a | format : Maybe String } -> CliMonad (Elm.Expression -> Elm.Expression)
basicTypeToEncoder basicType { format } =
    let
        default : Elm.Expression -> Elm.Expression
        default =
            case basicType of
                Common.String ->
                    Gen.Json.Encode.call_.string

                Common.Integer ->
                    Gen.Json.Encode.call_.int

                Common.Number ->
                    Gen.Json.Encode.call_.float

                Common.Boolean ->
                    Gen.Json.Encode.call_.bool
    in
    CliMonad.withFormat basicType format .encode default


oneOfAnnotation : Bool -> Common.TypeName -> Common.OneOfData -> CliMonad Elm.Annotation.Annotation
oneOfAnnotation qualify oneOfName oneOfData =
    CliMonad.andThen
        (\typesNamespace ->
            Elm.Annotation.named
                (if qualify then
                    typesNamespace

                 else
                    []
                )
                oneOfName
                |> CliMonad.succeedWith
                    (FastDict.singleton oneOfName oneOfData)
        )
        (CliMonad.moduleToNamespace Common.Types)


refToTypeName : List String -> CliMonad Common.UnsafeName
refToTypeName ref =
    case ref of
        [ "#", "components", _, name ] ->
            CliMonad.succeed (Common.UnsafeName name)

        _ ->
            CliMonad.fail <| "Couldn't get the type ref (" ++ String.join "/" ref ++ ") for the response"


typeToDecoder : Bool -> Common.Type -> CliMonad Elm.Expression
typeToDecoder qualify type_ =
    case type_ of
        Common.Object properties ->
            List.foldl
                (\( key, field ) prevExprRes ->
                    CliMonad.map2
                        (\internalDecoder prevExpr ->
                            Elm.Op.pipe
                                (Elm.apply
                                    Gen.OpenApi.Common.values_.jsonDecodeAndMap
                                    [ if field.required then
                                        Gen.Json.Decode.field (Common.unwrapUnsafe key) internalDecoder

                                      else
                                        Gen.OpenApi.Common.decodeOptionalField
                                            (Common.unwrapUnsafe key)
                                            internalDecoder
                                    ]
                                )
                                prevExpr
                        )
                        (typeToDecoder qualify field.type_)
                        prevExprRes
                )
                (CliMonad.succeed
                    (Gen.Json.Decode.succeed
                        (Elm.function
                            (List.map (\( key, _ ) -> ( Common.toValueName key, Nothing )) properties)
                            (\args ->
                                Elm.record
                                    (List.map2
                                        (\( key, _ ) arg -> ( Common.toValueName key, arg ))
                                        properties
                                        args
                                    )
                            )
                        )
                    )
                )
                properties

        Common.Basic basicType basic ->
            basicTypeToDecoder basicType basic

        Common.Null ->
            CliMonad.succeed (Gen.Json.Decode.null Elm.unit)

        Common.Unit ->
            CliMonad.succeed (Gen.Json.Decode.succeed Elm.unit)

        Common.List t ->
            CliMonad.map Gen.Json.Decode.list
                (typeToDecoder qualify t)

        Common.Dict additionalProperties [] ->
            CliMonad.map Gen.Json.Decode.dict
                (typeToDecoder qualify additionalProperties.type_)

        Common.Dict additionalProperties properties ->
            properties
                |> List.foldl
                    (\( key, field ) prevExprRes ->
                        CliMonad.map2
                            (\internalDecoder prevExpr ->
                                Elm.Op.pipe
                                    (Elm.apply
                                        Gen.OpenApi.Common.values_.jsonDecodeAndMap
                                        [ if field.required then
                                            Gen.Json.Decode.field (Common.unwrapUnsafe key) internalDecoder

                                          else
                                            Gen.OpenApi.Common.decodeOptionalField
                                                (Common.unwrapUnsafe key)
                                                internalDecoder
                                        ]
                                    )
                                    prevExpr
                            )
                            (typeToDecoder qualify field.type_)
                            prevExprRes
                    )
                    (CliMonad.succeed
                        (Gen.Json.Decode.succeed
                            (Elm.function
                                (List.map (\( key, _ ) -> ( Common.toValueName key, Nothing )) properties
                                    ++ [ ( Common.toValueName (Common.UnsafeName "additionalProperties"), Nothing ) ]
                                )
                                (\args ->
                                    Elm.record
                                        (List.map2
                                            (\( key, _ ) arg -> ( Common.toValueName key, arg ))
                                            (properties
                                                ++ [ ( Common.UnsafeName "additionalProperties"
                                                     , { type_ = additionalProperties.type_
                                                       , required = True
                                                       , documentation = Nothing
                                                       }
                                                     )
                                                   ]
                                            )
                                            args
                                        )
                                )
                            )
                        )
                    )
                |> CliMonad.map2
                    (\dictValueDecoder prevExpr ->
                        prevExpr
                            |> Elm.Op.pipe
                                (Elm.apply Gen.OpenApi.Common.values_.jsonDecodeAndMap
                                    [ Gen.Json.Decode.keyValuePairs Gen.Json.Decode.value
                                        |> Elm.Op.pipe
                                            (Elm.apply Gen.Json.Decode.values_.andThen
                                                [ Elm.fn (Elm.Arg.var "keyValuePairs")
                                                    (\keyValuePairs ->
                                                        Gen.List.call_.filterMap
                                                            (Elm.fn (Elm.Arg.tuple (Elm.Arg.var "key") (Elm.Arg.var "jsonValue"))
                                                                (\( key, jsonValue ) ->
                                                                    let
                                                                        propertyNames : Elm.Expression
                                                                        propertyNames =
                                                                            properties
                                                                                |> List.map (Tuple.first >> Common.unwrapUnsafe >> Elm.string)
                                                                                |> Elm.list
                                                                    in
                                                                    Elm.ifThen (Elm.apply Gen.List.values_.member [ key, propertyNames ])
                                                                        Elm.nothing
                                                                        (Elm.Let.letIn
                                                                            (\additionalPropertyDecoder ->
                                                                                Elm.Case.result
                                                                                    (Elm.apply Gen.Json.Decode.values_.decodeValue
                                                                                        [ additionalPropertyDecoder, jsonValue ]
                                                                                    )
                                                                                    { err =
                                                                                        ( "decodeError"
                                                                                        , \decodeError ->
                                                                                            let
                                                                                                decoderErrorAsString : Elm.Expression
                                                                                                decoderErrorAsString =
                                                                                                    Elm.apply Gen.Json.Decode.values_.errorToString
                                                                                                        [ decodeError ]
                                                                                            in
                                                                                            Elm.just
                                                                                                (Gen.Result.make_.err
                                                                                                    (decoderErrorAsString
                                                                                                        |> Elm.Op.append (Elm.string "': ")
                                                                                                        |> Elm.Op.append key
                                                                                                        |> Elm.Op.append (Elm.string "Field '")
                                                                                                    )
                                                                                                )
                                                                                        )
                                                                                    , ok =
                                                                                        ( "decodedValue"
                                                                                        , \decodedValue ->
                                                                                            Elm.just
                                                                                                (Gen.Result.make_.ok (Elm.tuple key decodedValue))
                                                                                        )
                                                                                    }
                                                                            )
                                                                            |> Elm.Let.value "additionalPropertyDecoder" dictValueDecoder
                                                                            |> Elm.Let.toExpression
                                                                        )
                                                                )
                                                            )
                                                            keyValuePairs
                                                            |> Elm.Op.pipe
                                                                (Elm.fn
                                                                    (Elm.Arg.var "resultPairs")
                                                                    (\resultPairs ->
                                                                        Elm.Let.letIn
                                                                            (\fieldErrors ->
                                                                                Elm.ifThen (Elm.apply Gen.List.values_.isEmpty [ fieldErrors ])
                                                                                    (resultPairs
                                                                                        |> Elm.Op.pipe
                                                                                            (Elm.apply Gen.List.values_.filterMap
                                                                                                [ Gen.Result.values_.toMaybe ]
                                                                                            )
                                                                                        |> Elm.Op.pipe Gen.Dict.values_.fromList
                                                                                        |> Elm.Op.pipe Gen.Json.Decode.values_.succeed
                                                                                    )
                                                                                    (Elm.list
                                                                                        [ Elm.string "Errors while decoding additionalProperties:\n- "
                                                                                        , Elm.apply Gen.String.values_.join
                                                                                            [ Elm.string "\n\n- ", fieldErrors ]
                                                                                        , Elm.string "\n"
                                                                                        ]
                                                                                        |> Elm.Op.pipe Gen.String.values_.concat
                                                                                        |> Elm.Op.pipe Gen.Json.Decode.values_.fail
                                                                                    )
                                                                            )
                                                                            |> Elm.Let.value "fieldErrors"
                                                                                (Elm.apply Gen.List.values_.filterMap
                                                                                    [ Elm.fn (Elm.Arg.var "field")
                                                                                        (\field ->
                                                                                            Elm.Case.result field
                                                                                                { ok = ( "_", \_ -> Elm.nothing )
                                                                                                , err = ( "error", \error -> Elm.just error )
                                                                                                }
                                                                                        )
                                                                                    , resultPairs
                                                                                    ]
                                                                                )
                                                                            |> Elm.Let.toExpression
                                                                    )
                                                                )
                                                    )
                                                ]
                                            )
                                    ]
                                )
                    )
                    (typeToDecoder qualify additionalProperties.type_)

        Common.Enum variants ->
            CliMonad.enumName variants
                |> CliMonad.andThen
                    (\maybeName ->
                        case maybeName of
                            Nothing ->
                                CliMonad.succeed Gen.Json.Decode.string

                            Just name ->
                                CliMonad.map
                                    (\jsonNamespace ->
                                        Elm.value
                                            { importFrom =
                                                if qualify then
                                                    jsonNamespace

                                                else
                                                    []
                                            , name = "decode" ++ Common.toTypeName name
                                            , annotation = Nothing
                                            }
                                    )
                                    (CliMonad.moduleToNamespace Common.Json)
                    )

        Common.Value ->
            CliMonad.succeed Gen.Json.Decode.value

        Common.Nullable t ->
            CliMonad.map
                (\decoder ->
                    Gen.Json.Decode.oneOf
                        [ Gen.Json.Decode.map
                            Gen.OpenApi.Common.make_.present
                            decoder
                        , Gen.Json.Decode.null
                            Gen.OpenApi.Common.make_.null
                        ]
                )
                (typeToDecoder qualify t)

        Common.Ref ref ->
            CliMonad.map2
                (\jsonNamespace name ->
                    Elm.value
                        { importFrom =
                            if qualify then
                                jsonNamespace

                            else
                                []
                        , name = "decode" ++ Common.toTypeName name
                        , annotation = Nothing
                        }
                )
                (CliMonad.moduleToNamespace Common.Json)
                (refToTypeName ref)

        Common.OneOf oneOfName variants ->
            variants
                |> CliMonad.combineMap
                    (\variant ->
                        typeToDecoder qualify variant.type_
                            |> CliMonad.map2
                                (\typesNamespace ->
                                    Gen.Json.Decode.call_.map
                                        (Elm.value
                                            { importFrom = typesNamespace
                                            , name = toVariantName oneOfName variant.name
                                            , annotation = Nothing
                                            }
                                        )
                                )
                                (CliMonad.moduleToNamespace Common.Types)
                    )
                |> CliMonad.map2
                    (\typesNamespace decoders ->
                        decoders
                            |> Gen.Json.Decode.oneOf
                            |> Elm.withType (Elm.Annotation.named typesNamespace oneOfName)
                    )
                    (CliMonad.moduleToNamespace Common.Types)

        Common.Bytes ->
            CliMonad.todo "Bytes decoder not implemented yet"


basicTypeToDecoder : Common.BasicType -> { format : Maybe String, const : Maybe Common.ConstValue } -> CliMonad Elm.Expression
basicTypeToDecoder basicType { format, const } =
    let
        base : (Elm.Expression -> Elm.Expression) -> Elm.Expression -> Elm.Expression
        base toString decoder =
            case const of
                Nothing ->
                    decoder

                Just expected ->
                    decoder
                        |> Gen.Json.Decode.andThen
                            (\actual ->
                                Elm.ifThen
                                    (Elm.Op.equal actual (constToExpr expected))
                                    (Gen.Json.Decode.succeed actual)
                                    (Gen.Json.Decode.call_.fail
                                        (Elm.Op.append
                                            (Elm.string
                                                ("Unexpected value: expected "
                                                    ++ constToString expected
                                                    ++ " got "
                                                )
                                            )
                                            (toString actual)
                                        )
                                    )
                            )

        default : Elm.Expression
        default =
            case basicType of
                Common.String ->
                    base
                        (\s -> Gen.Json.Encode.encode 0 (Gen.Json.Encode.call_.string s))
                        Gen.Json.Decode.string

                Common.Integer ->
                    base Gen.String.call_.fromInt Gen.Json.Decode.int

                Common.Number ->
                    base Gen.String.call_.fromFloat Gen.Json.Decode.float

                Common.Boolean ->
                    let
                        boolToString : Bool -> String
                        boolToString b =
                            if b then
                                "true"

                            else
                                "false"
                    in
                    case const of
                        Just (Common.ConstBoolean expected) ->
                            Gen.Json.Decode.bool
                                |> Gen.Json.Decode.andThen
                                    (\actual ->
                                        Elm.ifThen
                                            (Elm.Op.equal actual (Elm.bool expected))
                                            (Gen.Json.Decode.succeed actual)
                                            (Gen.Json.Decode.call_.fail
                                                (Elm.string
                                                    ("Unexpected value: expected "
                                                        ++ boolToString expected
                                                        ++ " got "
                                                        ++ boolToString (not expected)
                                                    )
                                                )
                                            )
                                    )

                        Just _ ->
                            Gen.Json.Decode.bool

                        Nothing ->
                            Gen.Json.Decode.bool
    in
    CliMonad.withFormat basicType format .decoder default


constToString : Common.ConstValue -> String
constToString const =
    case const of
        Common.ConstInteger i ->
            String.fromInt i

        Common.ConstBoolean True ->
            "true"

        Common.ConstBoolean False ->
            "false"

        Common.ConstNumber f ->
            String.fromFloat f

        Common.ConstString s ->
            Json.Encode.encode 0 (Json.Encode.string s)


constToExpr : Common.ConstValue -> Elm.Expression
constToExpr const =
    case const of
        Common.ConstInteger i ->
            Elm.int i

        Common.ConstBoolean b ->
            Elm.bool b

        Common.ConstString s ->
            Elm.string s

        Common.ConstNumber f ->
            Elm.float f


{-| Decode an optional field

    decodeString (decodeOptionalField "x" int) "{ \"x\": 3 }"
    --> Ok (Just 3)

    decodeString (decodeOptionalField "x" int) "{ \"x\": true }"
    --> Err ...

    decodeString (decodeOptionalField "x" int) "{ \"y\": 4 }"
    --> Ok Nothing

-}
decodeOptionalField : Elm.Declare.Function (Elm.Expression -> Elm.Expression -> Elm.Expression)
decodeOptionalField =
    let
        decoderAnnotation : Elm.Annotation.Annotation
        decoderAnnotation =
            Gen.Json.Decode.annotation_.decoder (Elm.Annotation.var "t")

        resultAnnotation : Elm.Annotation.Annotation
        resultAnnotation =
            Gen.Json.Decode.annotation_.decoder (Gen.Maybe.annotation_.maybe <| Elm.Annotation.var "t")
    in
    Elm.Declare.fn2 "decodeOptionalField"
        (Elm.Arg.varWith "key" Elm.Annotation.string)
        (Elm.Arg.varWith "fieldDecoder" decoderAnnotation)
    <|
        \key fieldDecoder ->
            -- The tricky part is that we want to make sure that
            -- if the field exists we error out if it has an incorrect shape.
            -- So what we do is we `oneOf` with `value` to avoid the `Nothing` branch,
            -- `andThen` we decode it. This is why we can't just use `maybe`, we would
            -- give `Nothing` when the shape is wrong.
            Gen.Json.Decode.oneOf
                [ Gen.Json.Decode.call_.map
                    (Elm.fn Elm.Arg.ignore <| \_ -> Elm.bool True)
                    (Gen.Json.Decode.call_.field key Gen.Json.Decode.value)
                , Gen.Json.Decode.succeed (Elm.bool False)
                ]
                |> Gen.Json.Decode.andThen
                    (\hasField ->
                        Elm.ifThen hasField
                            (Gen.Json.Decode.call_.field key
                                (Gen.Json.Decode.oneOf
                                    [ Gen.Json.Decode.map Gen.Maybe.make_.just fieldDecoder
                                    , Gen.Json.Decode.null Gen.Maybe.make_.nothing
                                    ]
                                )
                            )
                            (Gen.Json.Decode.succeed Gen.Maybe.make_.nothing)
                    )
                |> Elm.withType resultAnnotation


decodeOptionalFieldDocumentation : String
decodeOptionalFieldDocumentation =
    """Decode an optional field

    decodeString (decodeOptionalField "x" int) "{ "x": 3 }"
    --> Ok (Just 3)

    decodeString (decodeOptionalField "x" int) "{ "x": true }"
    --> Err ...

    decodeString (decodeOptionalField "x" int) "{ "y": 4 }"
    --> Ok Nothing"""
