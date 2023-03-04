module Cli exposing (run)

import Ansi.Color
import BackendTask
import BackendTask.File
import Cli.Option
import Cli.OptionsParser
import Cli.Program
import CliMonad exposing (CliMonad)
import Dict exposing (Dict)
import Elm
import Elm.Annotation
import Elm.Case
import Elm.Op
import Elm.ToString
import FatalError
import Gen.Basics
import Gen.Bytes
import Gen.Debug
import Gen.Http
import Gen.Json.Decode
import Gen.Json.Decode.Extra
import Gen.Json.Encode
import Gen.List
import Gen.Maybe
import Gen.Result
import Gen.String
import Gen.Url.Builder
import Json.Decode
import Json.Schema.Definitions
import List.Extra
import OpenApi
import OpenApi.Components
import OpenApi.Info
import OpenApi.MediaType
import OpenApi.Operation
import OpenApi.Parameter
import OpenApi.Path
import OpenApi.Reference
import OpenApi.RequestBody
import OpenApi.Response
import OpenApi.Schema
import OpenApi.SecurityRequirement
import OpenApi.Server
import Pages.Script
import Path
import Result.Extra
import String.Extra


type alias CliOptions =
    { entryFilePath : String
    , outputFile : Maybe String
    , namespace : Maybe String
    , generateTodos : Maybe String
    }


type alias Mime =
    String


type ContentSchema
    = EmptyContent
    | JsonContent Type
    | StringContent Mime
    | BytesContent Mime


type alias AuthorizationInfo =
    { headers : Elm.Expression -> List Elm.Expression
    , params : List ( String, Elm.Annotation.Annotation )
    , scopes : List String
    }


type Type
    = Nullable Type
    | Object (Dict String Type)
    | String
    | Int
    | Float
    | Bool
    | List Type
    | Enum (List Type)
    | Value
    | Named TypeName
    | Bytes
    | Unit


type TypeName
    = TypeName String


program : Cli.Program.Config CliOptions
program =
    Cli.Program.config
        |> Cli.Program.add
            (Cli.OptionsParser.build CliOptions
                |> Cli.OptionsParser.with
                    (Cli.Option.requiredPositionalArg "entryFilePath")
                |> Cli.OptionsParser.with
                    (Cli.Option.optionalKeywordArg "output")
                |> Cli.OptionsParser.with
                    (Cli.Option.optionalKeywordArg "namespace")
                |> Cli.OptionsParser.with
                    (Cli.Option.optionalKeywordArg "generateTodos")
            )


run : Pages.Script.Script
run =
    Pages.Script.withCliOptions program
        (\{ entryFilePath, outputFile, namespace, generateTodos } ->
            BackendTask.File.rawFile entryFilePath
                |> BackendTask.mapError
                    (\error ->
                        FatalError.fromString <|
                            Ansi.Color.fontColor Ansi.Color.brightRed <|
                                case error.recoverable of
                                    BackendTask.File.FileDoesntExist ->
                                        "Uh oh! There is no file at " ++ entryFilePath

                                    BackendTask.File.FileReadError str ->
                                        "Uh oh! Can't read!"

                                    BackendTask.File.DecodingError decodeErr ->
                                        "Uh oh! Decoding failure!"
                    )
                |> BackendTask.andThen decodeOpenApiSpecOrFail
                |> BackendTask.andThen (generateFileFromOpenApiSpec { outputFile = outputFile, namespace = namespace, generateTodos = generateTodos })
        )


decodeOpenApiSpecOrFail : String -> BackendTask.BackendTask FatalError.FatalError OpenApi.OpenApi
decodeOpenApiSpecOrFail =
    Json.Decode.decodeString OpenApi.decode
        >> Result.mapError
            (Json.Decode.errorToString
                >> Ansi.Color.fontColor Ansi.Color.brightRed
                >> FatalError.fromString
            )
        >> BackendTask.fromResult


generateFileFromOpenApiSpec :
    { outputFile : Maybe String
    , namespace : Maybe String
    , generateTodos : Maybe String
    }
    -> OpenApi.OpenApi
    -> BackendTask.BackendTask FatalError.FatalError ()
generateFileFromOpenApiSpec { outputFile, namespace, generateTodos } apiSpec =
    let
        fileNamespace : String
        fileNamespace =
            case namespace of
                Just n ->
                    n

                Nothing ->
                    let
                        defaultNamespace =
                            apiSpec
                                |> OpenApi.info
                                |> OpenApi.Info.title
                                |> makeNamespaceValid
                                |> removeInvalidChars
                    in
                    case outputFile of
                        Just path ->
                            let
                                split : List String
                                split =
                                    String.split "/" path
                            in
                            case List.Extra.dropWhile ((/=) "generated") split of
                                "generated" :: rest ->
                                    rest
                                        |> String.join "."
                                        |> String.replace ".elm" ""

                                _ ->
                                    case List.Extra.dropWhile ((/=) "src") split of
                                        "src" :: rest ->
                                            rest
                                                |> String.join "."
                                                |> String.replace ".elm" ""

                                        _ ->
                                            defaultNamespace

                        Nothing ->
                            defaultNamespace

        declarations =
            let
                combined =
                    CliMonad.combine
                        [ pathDeclarations
                        , CliMonad.succeed [ nullableType ]
                        , componentDeclarations
                        , responsesDeclarations
                        , helperDeclarations
                        ]
            in
            case
                CliMonad.run
                    { openApi = apiSpec
                    , generateTodos = List.member (String.toLower <| Maybe.withDefault "no" generateTodos) [ "y", "yes", "true" ]
                    }
                    combined
            of
                Ok ( decls, warnings ) ->
                    BackendTask.combine (List.map (\warning -> Pages.Script.log <| Ansi.Color.fontColor Ansi.Color.brightYellow "Warning: " ++ Ansi.Color.end Ansi.Color.Font ++ warning) warnings)
                        |> BackendTask.map (\_ -> List.concat decls)

                Err e ->
                    BackendTask.fail (FatalError.fromString e)

        file =
            BackendTask.map
                (Elm.fileWith [ fileNamespace ]
                    { docs =
                        List.sortBy
                            (\{ group } ->
                                case group of
                                    Just "Request functions" ->
                                        1

                                    Just "Types" ->
                                        2

                                    Just "Encoders" ->
                                        3

                                    Just "Decoders" ->
                                        4

                                    _ ->
                                        5
                            )
                            >> List.map Elm.docs
                    , aliases = []
                    }
                )
                declarations
    in
    file
        |> BackendTask.andThen
            (\{ contents, path } ->
                let
                    outputPath =
                        Maybe.withDefault
                            ([ "generated", path ]
                                |> Path.join
                                |> Path.toRelative
                            )
                            outputFile
                in
                Pages.Script.writeFile
                    { path = outputPath
                    , body = contents
                    }
                    |> BackendTask.mapError
                        (\error ->
                            FatalError.fromString <|
                                Ansi.Color.fontColor Ansi.Color.brightRed <|
                                    case error.recoverable of
                                        Pages.Script.FileWriteError ->
                                            "Uh oh! Failed to write file"
                        )
                    |> BackendTask.map (\_ -> outputPath)
            )
        |> BackendTask.andThen (\outputPath -> Pages.Script.log ("SDK generated at " ++ outputPath))


pathDeclarations : CliMonad (List Elm.Declaration)
pathDeclarations =
    CliMonad.fromApiSpec OpenApi.paths
        |> CliMonad.andThen
            (\paths ->
                paths
                    |> Dict.toList
                    |> CliMonad.combineMap
                        (\( url, path ) ->
                            [ ( "GET", OpenApi.Path.get )
                            , ( "POST", OpenApi.Path.post )
                            , ( "PUT", OpenApi.Path.put )
                            , ( "PATCH", OpenApi.Path.patch )
                            , ( "DELETE", OpenApi.Path.delete )
                            , ( "HEAD", OpenApi.Path.head )
                            , ( "TRACE", OpenApi.Path.trace )
                            ]
                                |> List.filterMap (\( method, getter ) -> Maybe.map (Tuple.pair method) (getter path))
                                |> CliMonad.combineMap
                                    (\( method, operation ) ->
                                        toRequestFunction method url operation
                                            |> CliMonad.errorToWarning
                                    )
                                |> CliMonad.map (List.filterMap identity)
                        )
                    |> CliMonad.map
                        (List.concat
                            >> List.map
                                (Elm.exposeWith
                                    { exposeConstructor = False
                                    , group = Just "Request functions"
                                    }
                                )
                        )
            )


responsesDeclarations : CliMonad (List Elm.Declaration)
responsesDeclarations =
    CliMonad.fromApiSpec
        (OpenApi.components
            >> Maybe.map OpenApi.Components.responses
            >> Maybe.withDefault Dict.empty
        )
        |> CliMonad.andThen
            (Dict.foldl
                (\name schema ->
                    CliMonad.map2 (::)
                        (responseToDeclarations name schema)
                )
                (CliMonad.succeed [])
            )
        |> CliMonad.map List.concat


componentDeclarations : CliMonad (List Elm.Declaration)
componentDeclarations =
    CliMonad.fromApiSpec
        (OpenApi.components
            >> Maybe.map OpenApi.Components.schemas
            >> Maybe.withDefault Dict.empty
        )
        |> CliMonad.andThen
            (Dict.foldl
                (\name schema ->
                    CliMonad.map2 (::)
                        (schemaToDeclarations name (OpenApi.Schema.get schema))
                )
                (CliMonad.succeed [])
            )
        |> CliMonad.map List.concat


helperDeclarations : CliMonad (List Elm.Declaration)
helperDeclarations =
    -- The max value here should match with the max value supported by `intToWord`
    List.range 1 99
        |> List.map
            (\i ->
                intToWord i
                    |> Result.andThen
                        (\intWord ->
                            let
                                (TypeName typeName) =
                                    typifyName ("enum_" ++ intWord)
                            in
                            List.range 1 i
                                |> List.foldr
                                    (\j res ->
                                        Result.map2
                                            (\jWord r ->
                                                let
                                                    (TypeName variantName) =
                                                        typifyName ("enum_" ++ intWord ++ "_" ++ jWord)
                                                in
                                                Elm.variantWith variantName [ Elm.Annotation.var (toValueName jWord) ]
                                                    :: r
                                            )
                                            (intToWord j)
                                            res
                                    )
                                    (Ok [])
                                |> Result.map (Elm.customType typeName)
                        )
            )
        |> Result.Extra.combine
        |> CliMonad.fromResult


schemaToDeclarations : String -> Json.Schema.Definitions.Schema -> CliMonad (List Elm.Declaration)
schemaToDeclarations name schema =
    schemaToAnnotation schema
        |> CliMonad.andThen
            (\ann ->
                let
                    (TypeName typeName) =
                        typifyName name
                in
                if (Elm.ToString.annotation ann).signature == typeName then
                    CliMonad.succeed []

                else
                    [ Elm.alias typeName ann
                        |> Elm.exposeWith
                            { exposeConstructor = False
                            , group = Just "Types"
                            }
                        |> CliMonad.succeed
                    , CliMonad.map
                        (\schemaDecoder ->
                            Elm.declaration ("decode" ++ typeName)
                                (schemaDecoder
                                    |> Elm.withType (Gen.Json.Decode.annotation_.decoder (Elm.Annotation.named [] typeName))
                                )
                                |> Elm.exposeWith
                                    { exposeConstructor = False
                                    , group = Just "Decoders"
                                    }
                        )
                        (schemaToDecoder schema)
                    , CliMonad.map
                        (\encoder ->
                            Elm.declaration ("encode" ++ typeName)
                                (encoder
                                    |> Elm.withType (Elm.Annotation.function [ Elm.Annotation.named [] typeName ] Gen.Json.Encode.annotation_.value)
                                )
                                |> Elm.exposeWith
                                    { exposeConstructor = False
                                    , group = Just "Encoders"
                                    }
                        )
                        (schemaToEncoder schema)
                    ]
                        |> CliMonad.combine
            )
        |> CliMonad.withPath name


responseToDeclarations : String -> OpenApi.Reference.ReferenceOr OpenApi.Response.Response -> CliMonad (List Elm.Declaration)
responseToDeclarations name reference =
    case OpenApi.Reference.toConcrete reference of
        Just response ->
            let
                content =
                    OpenApi.Response.content response
            in
            if Dict.isEmpty content then
                -- If there is no content then we go with the unit value, `()` as the response type
                CliMonad.succeed []

            else
                responseToSchema response
                    |> CliMonad.withPath name
                    |> CliMonad.andThen (schemaToDeclarations name)

        Nothing ->
            CliMonad.fail "Could not convert reference to concrete value"
                |> CliMonad.withPath name


stepOrFail : String -> (a -> Maybe value) -> CliMonad a -> CliMonad value
stepOrFail msg f =
    CliMonad.andThen
        (\y ->
            case f y of
                Just z ->
                    CliMonad.succeed z

                Nothing ->
                    CliMonad.fail msg
        )


toRequestFunction : String -> String -> OpenApi.Operation.Operation -> CliMonad Elm.Declaration
toRequestFunction method url operation =
    operationToSuccessTypeAndExpect operation
        |> CliMonad.andThen
            (\( successType, toExpect ) ->
                let
                    functionName : String
                    functionName =
                        (OpenApi.Operation.operationId operation
                            |> Maybe.withDefault url
                        )
                            |> makeNamespaceValid
                            |> removeInvalidChars
                            |> String.Extra.camelize

                    replacedUrl : CliMonad (Elm.Expression -> Elm.Expression)
                    replacedUrl =
                        let
                            params =
                                OpenApi.Operation.parameters operation
                        in
                        params
                            |> CliMonad.combineMap
                                (\param ->
                                    toConcreteParam param
                                        |> CliMonad.andThen
                                            (\concreteParam ->
                                                case OpenApi.Parameter.in_ concreteParam of
                                                    "path" ->
                                                        if OpenApi.Parameter.required concreteParam then
                                                            let
                                                                pname : String
                                                                pname =
                                                                    OpenApi.Parameter.name concreteParam
                                                            in
                                                            ( Just
                                                                (\config ->
                                                                    Gen.String.call_.replace
                                                                        (Elm.string <| "{" ++ pname ++ "}")
                                                                        (Elm.get (toValueName pname) (Elm.get "params" config))
                                                                )
                                                            , []
                                                            )
                                                                |> CliMonad.succeed

                                                        else
                                                            CliMonad.fail "Optional parameters in path"

                                                    "query" ->
                                                        CliMonad.succeed ( Nothing, [ concreteParam ] )

                                                    paramIn ->
                                                        CliMonad.todoWithDefault ( Nothing, [] ) <| "Parameters in \"" ++ paramIn ++ "\""
                                            )
                                )
                            |> CliMonad.andThen
                                (\pairs ->
                                    let
                                        fullUrl : CliMonad String
                                        fullUrl =
                                            CliMonad.fromApiSpec OpenApi.servers
                                                |> CliMonad.map
                                                    (\servers ->
                                                        case servers of
                                                            [] ->
                                                                url

                                                            firstServer :: _ ->
                                                                if String.startsWith "/" url then
                                                                    OpenApi.Server.url firstServer ++ url

                                                                else
                                                                    OpenApi.Server.url firstServer ++ "/" ++ url
                                                    )

                                        ( replacements, queryParams ) =
                                            List.unzip pairs
                                                |> Tuple.mapBoth (List.filterMap identity) List.concat

                                        replaced : CliMonad (Elm.Expression -> Elm.Expression)
                                        replaced =
                                            fullUrl
                                                |> CliMonad.map
                                                    (\u ->
                                                        \config ->
                                                            List.foldl
                                                                (\replacement -> replacement config)
                                                                (Elm.string u)
                                                                replacements
                                                    )
                                    in
                                    if List.isEmpty queryParams then
                                        replaced

                                    else
                                        queryParams
                                            |> CliMonad.combineMap queryParameterToUrlBuilderArgument
                                            |> CliMonad.map2
                                                (\repl queryArgs config ->
                                                    queryArgs
                                                        |> List.map (\arg -> arg config)
                                                        |> Gen.List.filterMap Gen.Basics.identity
                                                        |> Gen.Url.Builder.call_.crossOrigin
                                                            (repl config)
                                                            (Elm.list [])
                                                )
                                                replaced
                                )

                    body : ContentSchema -> CliMonad (Elm.Expression -> Elm.Expression)
                    body bodyContent =
                        case bodyContent of
                            EmptyContent ->
                                CliMonad.succeed (\_ -> Gen.Http.emptyBody)

                            JsonContent type_ ->
                                typeToEncoder type_
                                    |> CliMonad.map
                                        (\encoder config ->
                                            Gen.Http.jsonBody
                                                (Elm.apply encoder [ Elm.get "body" config ])
                                        )

                            StringContent mime ->
                                CliMonad.succeed <| \config -> Gen.Http.call_.stringBody (Elm.string mime) (Elm.get "body" config)

                            BytesContent mime ->
                                CliMonad.succeed <| \config -> Gen.Http.bytesBody mime (Elm.get "body" config)

                    expect : Elm.Expression -> Elm.Expression
                    expect config =
                        toExpect (\toMsgArg -> Elm.apply (Elm.get "toMsg" config) [ toMsgArg ])

                    bodyParams : ContentSchema -> CliMonad (List ( String, Elm.Annotation.Annotation ))
                    bodyParams contentSchema =
                        case contentSchema of
                            EmptyContent ->
                                CliMonad.succeed []

                            JsonContent type_ ->
                                typeToAnnotation type_
                                    |> CliMonad.map (\annotation -> [ ( "body", annotation ) ])

                            StringContent _ ->
                                CliMonad.succeed [ ( "body", Elm.Annotation.string ) ]

                            BytesContent _ ->
                                CliMonad.succeed [ ( "body", Gen.Bytes.annotation_.bytes ) ]

                    function : ContentSchema -> CliMonad Elm.Expression
                    function bodyContent =
                        authorizationInfo
                            |> CliMonad.andThen
                                (\auth ->
                                    CliMonad.map3
                                        (\toBody param replaced ->
                                            Elm.fn
                                                param
                                                (\config ->
                                                    Gen.Http.call_.request
                                                        (Elm.record
                                                            [ ( "method", Elm.string method )
                                                            , ( "headers", Elm.list <| auth.headers config )
                                                            , ( "expect", expect config )
                                                            , ( "body", toBody config )
                                                            , ( "timeout", Gen.Maybe.make_.nothing )
                                                            , ( "tracker", Gen.Maybe.make_.nothing )
                                                            , ( "url", replaced config )
                                                            ]
                                                        )
                                                )
                                        )
                                        (body bodyContent)
                                        (bodyParams bodyContent
                                            |> CliMonad.andThen
                                                (\bp ->
                                                    typeToAnnotation successType
                                                        |> CliMonad.andThen
                                                            (\successAnnotation ->
                                                                toConfigParam operation successAnnotation auth bp
                                                            )
                                                )
                                        )
                                        replacedUrl
                                )

                    authorizationInfo =
                        operationToAuthorizationInfo operation

                    documentation : CliMonad String
                    documentation =
                        authorizationInfo
                            |> CliMonad.map
                                (\{ scopes } ->
                                    OpenApi.Operation.description operation
                                        |> Maybe.withDefault ""
                                        |> (\d ->
                                                if List.isEmpty scopes then
                                                    d

                                                else
                                                    ([ d
                                                     , ""
                                                     , "This operations requires the following scopes:"
                                                     ]
                                                        ++ List.map
                                                            (\scope ->
                                                                " - `" ++ scope ++ "`"
                                                            )
                                                            scopes
                                                    )
                                                        |> String.join "\n"
                                           )
                                )
                in
                operationToContentSchema operation
                    |> CliMonad.andThen function
                    |> CliMonad.map2
                        (\doc fun ->
                            fun
                                |> Elm.declaration functionName
                                |> Elm.withDocumentation doc
                        )
                        documentation
            )
        |> CliMonad.withPath method
        |> CliMonad.withPath url


operationToAuthorizationInfo : OpenApi.Operation.Operation -> CliMonad AuthorizationInfo
operationToAuthorizationInfo operation =
    let
        empty : AuthorizationInfo
        empty =
            { headers = \_ -> []
            , params = []
            , scopes = []
            }
    in
    case
        List.map
            (Dict.toList << OpenApi.SecurityRequirement.requirements)
            (OpenApi.Operation.security operation)
    of
        [] ->
            CliMonad.succeed empty

        [ [ ( "oauth_2_0", ss ) ] ] ->
            CliMonad.succeed
                { headers =
                    \config ->
                        [ Gen.Http.call_.header (Elm.string "Authorization")
                            (Elm.Op.append
                                (Elm.string "Bearer ")
                                (config
                                    |> Elm.get "authorization"
                                    |> Elm.get "bearer"
                                )
                            )
                        ]
                , params =
                    [ ( "authorization"
                      , Elm.Annotation.record
                            [ ( "bearer"
                              , Elm.Annotation.string
                              )
                            ]
                      )
                    ]
                , scopes = ss
                }

        _ ->
            CliMonad.todoWithDefault empty "Multiple security requirements"


operationToContentSchema : OpenApi.Operation.Operation -> CliMonad ContentSchema
operationToContentSchema operation =
    case OpenApi.Operation.requestBody operation of
        Nothing ->
            CliMonad.succeed EmptyContent

        Just requestOrRef ->
            case OpenApi.Reference.toConcrete requestOrRef of
                Just request ->
                    OpenApi.RequestBody.content request
                        |> contentToContentSchema

                Nothing ->
                    CliMonad.succeed requestOrRef
                        |> stepOrFail "I found a successfull response, but I couldn't convert it to a concrete one"
                            OpenApi.Reference.toReference
                        |> CliMonad.map OpenApi.Reference.ref
                        |> CliMonad.andThen schemaTypeRef
                        |> CliMonad.map (\typeName -> JsonContent (Named typeName))


contentToContentSchema : Dict String OpenApi.MediaType.MediaType -> CliMonad ContentSchema
contentToContentSchema content =
    let
        default : Maybe (CliMonad ContentSchema) -> CliMonad ContentSchema
        default fallback =
            case Dict.get "application/json" content of
                Just jsonSchema ->
                    CliMonad.succeed jsonSchema
                        |> stepOrFail "The request's application/json content option doesn't have a schema"
                            (OpenApi.MediaType.schema >> Maybe.map OpenApi.Schema.get)
                        |> CliMonad.andThen schemaToType
                        |> CliMonad.map JsonContent

                Nothing ->
                    case Dict.get "text/html" content of
                        Just htmlSchema ->
                            stringContent "text/html" htmlSchema

                        Nothing ->
                            case Dict.get "text/plain" content of
                                Just htmlSchema ->
                                    stringContent "text/plain" htmlSchema

                                Nothing ->
                                    let
                                        msg =
                                            "The content doesn't have an application/json, text/html or text/plain option, it has " ++ String.join ", " (Dict.keys content)
                                    in
                                    fallback
                                        |> Maybe.withDefault (CliMonad.fail msg)

        stringContent mime htmlSchema =
            CliMonad.succeed htmlSchema
                |> stepOrFail ("The request's " ++ mime ++ " content option doesn't have a schema")
                    (OpenApi.MediaType.schema >> Maybe.map OpenApi.Schema.get)
                |> CliMonad.andThen schemaToType
                |> CliMonad.andThen
                    (\type_ ->
                        if type_ == String then
                            CliMonad.succeed (StringContent mime)

                        else
                            CliMonad.fail ("The only supported type for " ++ mime ++ " content is string")
                    )
    in
    case Dict.toList content of
        [] ->
            CliMonad.succeed EmptyContent

        [ ( singleKey, singleValue ) ] ->
            if singleKey == "application/octet-stream" then
                CliMonad.succeed (BytesContent singleKey)

            else
                let
                    fallback : CliMonad ContentSchema
                    fallback =
                        CliMonad.succeed
                            (BytesContent singleKey)
                            |> CliMonad.withWarning ("Unrecognized mime type: " ++ singleKey ++ ", treating it as bytes")
                in
                if String.startsWith "image/" singleKey then
                    -- TODO: handle base64
                    default (Just fallback)

                else
                    default (Just fallback)

        _ ->
            default Nothing


toConfigParam : OpenApi.Operation.Operation -> Elm.Annotation.Annotation -> AuthorizationInfo -> List ( String, Elm.Annotation.Annotation ) -> CliMonad ( String, Maybe Elm.Annotation.Annotation )
toConfigParam operation successType authorizationInfo bodyParams =
    CliMonad.map
        (\urlParams ->
            ( "config"
            , (authorizationInfo.params
                ++ ( "toMsg"
                   , Elm.Annotation.function
                        [ Gen.Result.annotation_.result Gen.Http.annotation_.error successType ]
                        (Elm.Annotation.var "msg")
                   )
                :: bodyParams
                ++ urlParams
              )
                |> recordType
                |> Just
            )
        )
        (operationToUrlParams operation)


operationToUrlParams : OpenApi.Operation.Operation -> CliMonad (List ( String, Elm.Annotation.Annotation ))
operationToUrlParams operation =
    let
        params =
            OpenApi.Operation.parameters operation
    in
    if List.isEmpty params then
        CliMonad.succeed []

    else
        params
            |> CliMonad.combineMap
                (\param ->
                    toConcreteParam param
                        |> CliMonad.andThen paramToAnnotation
                )
            |> CliMonad.map
                (\types -> [ ( "params", recordType types ) ])


recordType : List ( String, Elm.Annotation.Annotation ) -> Elm.Annotation.Annotation
recordType fields =
    fields
        |> List.map (Tuple.mapFirst toValueName)
        |> Elm.Annotation.record


queryParameterToUrlBuilderArgument : OpenApi.Parameter.Parameter -> CliMonad (Elm.Expression -> Elm.Expression)
queryParameterToUrlBuilderArgument param =
    paramToAnnotation param
        |> CliMonad.andThen
            (\( paramName, annotation ) ->
                let
                    name =
                        Elm.string paramName

                    value config =
                        Elm.get (toValueName paramName) (Elm.get "params" config)

                    paramBuilderHelper : List String -> CliMonad (Elm.Expression -> Elm.Expression)
                    paramBuilderHelper parts =
                        case parts of
                            "String" :: [] ->
                                identity
                                    |> CliMonad.succeed

                            "Int" :: [] ->
                                Gen.String.call_.fromInt
                                    |> CliMonad.succeed

                            "Float" :: [] ->
                                Gen.String.call_.fromFloat
                                    |> CliMonad.succeed

                            "Bool" :: [] ->
                                (\val ->
                                    Elm.ifThen val
                                        (Elm.string "true")
                                        (Elm.string "false")
                                )
                                    |> CliMonad.succeed

                            t ->
                                let
                                    msg : String
                                    msg =
                                        "Params of type \"" ++ String.join " " t ++ "\" (in helper)"
                                in
                                CliMonad.todoWithDefault (\_ -> Gen.Debug.todo msg) msg
                in
                case (Elm.ToString.annotation annotation).signature |> String.split " " of
                    [ a ] ->
                        paramBuilderHelper [ a ]
                            |> CliMonad.map
                                (\f config ->
                                    f (value config)
                                        |> Gen.Url.Builder.call_.string name
                                        |> Gen.Maybe.make_.just
                                )

                    "Maybe" :: rest ->
                        paramBuilderHelper rest
                            |> CliMonad.map (\f config -> Gen.Maybe.map (f >> Gen.Url.Builder.call_.string name) (value config))

                    "List" :: rest ->
                        paramBuilderHelper rest
                            |> CliMonad.map
                                (\f config ->
                                    Gen.Maybe.map
                                        (f
                                            >> Gen.String.call_.join (Elm.string ",")
                                            >> Gen.Url.Builder.call_.string name
                                        )
                                        (value config)
                                )

                    t ->
                        -- TODO: This seems to be mostly aliases, at least in the GitHub OAS
                        CliMonad.todo ("Params of type \"" ++ String.join " " t ++ "\"")
                            |> CliMonad.map (\e _ -> e)
            )


paramToAnnotation : OpenApi.Parameter.Parameter -> CliMonad ( String, Elm.Annotation.Annotation )
paramToAnnotation concreteParam =
    let
        pname : String
        pname =
            OpenApi.Parameter.name concreteParam
    in
    CliMonad.succeed concreteParam
        |> stepOrFail ("Could not get schema for parameter " ++ pname)
            (OpenApi.Parameter.schema >> Maybe.map OpenApi.Schema.get)
        |> CliMonad.andThen schemaToAnnotation
        |> CliMonad.map
            (\annotation ->
                ( pname
                , if OpenApi.Parameter.required concreteParam then
                    annotation

                  else
                    Elm.Annotation.maybe annotation
                )
            )


toConcreteParam : OpenApi.Reference.ReferenceOr OpenApi.Parameter.Parameter -> CliMonad OpenApi.Parameter.Parameter
toConcreteParam param =
    case OpenApi.Reference.toConcrete param of
        Just concreteParam ->
            CliMonad.succeed concreteParam

        Nothing ->
            CliMonad.succeed param
                |> stepOrFail "I found a params, but I couldn't convert it to a concrete one" OpenApi.Reference.toReference
                |> CliMonad.map OpenApi.Reference.ref
                |> CliMonad.andThen
                    (\ref ->
                        case String.split "/" ref of
                            [ "#", "components", "parameters", parameterType ] ->
                                CliMonad.fromApiSpec OpenApi.components
                                    |> CliMonad.andThen
                                        (\components ->
                                            components
                                                |> Maybe.map OpenApi.Components.parameters
                                                |> Maybe.andThen (Dict.get parameterType)
                                                |> Maybe.map toConcreteParam
                                                |> Maybe.withDefault (CliMonad.fail <| "Param ref " ++ parameterType ++ " not found")
                                        )

                            _ ->
                                CliMonad.fail <| "Param reference should be to \"#/components/parameters/ref\", found:" ++ ref
                    )


operationToSuccessTypeAndExpect : OpenApi.Operation.Operation -> CliMonad ( Type, (Elm.Expression -> Elm.Expression) -> Elm.Expression )
operationToSuccessTypeAndExpect operation =
    let
        responses : Dict.Dict String (OpenApi.Reference.ReferenceOr OpenApi.Response.Response)
        responses =
            OpenApi.Operation.responses operation

        expectJson : Elm.Expression -> ((Elm.Expression -> Elm.Expression) -> Elm.Expression)
        expectJson decoder toMsg =
            Gen.Http.expectJson toMsg decoder
    in
    CliMonad.succeed responses
        |> stepOrFail
            ("Among the "
                ++ String.fromInt (Dict.size responses)
                ++ " possible responses, there was no successfull one."
            )
            getFirstSuccessResponse
        |> CliMonad.andThen
            (\( _, responseOrRef ) ->
                case OpenApi.Reference.toConcrete responseOrRef of
                    Just response ->
                        OpenApi.Response.content response
                            |> contentToContentSchema
                            |> CliMonad.andThen
                                (\contentSchema ->
                                    case contentSchema of
                                        JsonContent type_ ->
                                            CliMonad.map
                                                (\decoder -> ( type_, expectJson decoder ))
                                                (typeToDecoder type_)

                                        StringContent _ ->
                                            CliMonad.succeed
                                                ( String
                                                , Gen.Http.expectString
                                                )

                                        BytesContent _ ->
                                            CliMonad.succeed
                                                ( Bytes
                                                , \toMsg -> Gen.Http.expectBytes toMsg Gen.Basics.values_.identity
                                                )

                                        EmptyContent ->
                                            CliMonad.succeed
                                                ( Unit
                                                , Gen.Http.expectWhatever
                                                )
                                )

                    Nothing ->
                        CliMonad.succeed responseOrRef
                            |> stepOrFail "I found a successfull response, but I couldn't convert it to a concrete one"
                                OpenApi.Reference.toReference
                            |> CliMonad.map OpenApi.Reference.ref
                            |> CliMonad.andThen schemaTypeRef
                            |> CliMonad.map
                                (\((TypeName typeName) as st) ->
                                    ( Named st
                                    , expectJson <| Elm.val ("decode" ++ typeName)
                                    )
                                )
            )


schemaTypeRef : String -> CliMonad TypeName
schemaTypeRef refUri =
    case String.split "/" refUri of
        [ "#", "components", "schemas", schemaName ] ->
            CliMonad.succeed (typifyName schemaName)

        [ "#", "components", "responses", responseName ] ->
            CliMonad.succeed (typifyName responseName)

        _ ->
            CliMonad.fail <| "Couldn't get the type ref (" ++ refUri ++ ") for the response"


getFirstSuccessResponse : Dict.Dict String (OpenApi.Reference.ReferenceOr OpenApi.Response.Response) -> Maybe ( String, OpenApi.Reference.ReferenceOr OpenApi.Response.Response )
getFirstSuccessResponse responses =
    responses
        |> Dict.toList
        |> List.filter (\( statusCode, _ ) -> String.startsWith "2" statusCode)
        |> List.head


nullableType : Elm.Declaration
nullableType =
    Elm.customType "Nullable"
        [ Elm.variant "Null"
        , Elm.variantWith "Present" [ Elm.Annotation.var "value" ]
        ]
        |> Elm.exposeWith
            { exposeConstructor = True
            , group = Just "Types"
            }


typifyName : String -> TypeName
typifyName name =
    name
        |> String.uncons
        |> Maybe.map (\( first, rest ) -> String.cons first (String.replace "-" " " rest))
        |> Maybe.withDefault ""
        |> String.replace "_" " "
        |> String.Extra.toTitleCase
        |> String.replace " " ""
        |> deSymbolify
        |> TypeName


toValueName : String -> String
toValueName name =
    name
        |> String.uncons
        |> Maybe.map (\( first, rest ) -> String.cons (Char.toLower first) (String.replace "-" "_" rest))
        |> Maybe.withDefault ""
        |> deSymbolify


{-| Sometimes a word in the schema contains invalid characers for an Elm name.
We don't want to completely remove them though.
-}
deSymbolify : String -> String
deSymbolify str =
    str
        |> String.replace "+" "Plus"
        |> String.replace "-" "Minus"


schemaToEncoder : Json.Schema.Definitions.Schema -> CliMonad Elm.Expression
schemaToEncoder schema =
    schemaToType schema |> CliMonad.andThen typeToEncoder


typeToEncoder : Type -> CliMonad Elm.Expression
typeToEncoder type_ =
    case type_ of
        String ->
            CliMonad.succeed Gen.Json.Encode.values_.string

        Int ->
            CliMonad.succeed Gen.Json.Encode.values_.int

        Float ->
            CliMonad.succeed Gen.Json.Encode.values_.float

        Bool ->
            CliMonad.succeed Gen.Json.Encode.values_.bool

        Object properties ->
            properties
                |> Dict.toList
                |> CliMonad.combineMap
                    (\( key, valueType ) ->
                        typeToEncoder valueType
                            |> CliMonad.map
                                (\encoder rec ->
                                    Elm.tuple
                                        (Elm.string key)
                                        (Elm.apply encoder [ Elm.get (toValueName key) rec ])
                                )
                    )
                |> CliMonad.map (\toProperties rec -> Gen.Json.Encode.object (List.map (\prop -> prop rec) toProperties))
                |> CliMonad.map
                    (\toEncoder ->
                        Elm.fn ( "rec", Nothing )
                            (\rec ->
                                toEncoder rec
                            )
                    )

        List t ->
            typeToEncoder t
                |> CliMonad.map
                    (\encoder ->
                        Elm.apply Gen.Json.Encode.values_.list [ encoder ]
                    )

        Nullable t ->
            typeToEncoder t
                |> CliMonad.map
                    (\encoder ->
                        Elm.fn ( "nullableValue", Nothing )
                            (\nullableValue ->
                                Elm.Case.custom
                                    nullableValue
                                    (Elm.Annotation.namedWith [] "Nullable" [ Elm.Annotation.var "value" ])
                                    [ Elm.Case.branch0 "Null" Gen.Json.Encode.null
                                    , Elm.Case.branch1 "Present"
                                        ( "value", Elm.Annotation.var "value" )
                                        (\value -> Elm.apply encoder [ value ])
                                    ]
                            )
                    )

        Value ->
            CliMonad.succeed <| Gen.Basics.values_.identity

        Named (TypeName name) ->
            CliMonad.succeed <| Elm.val ("encode" ++ name)

        Enum _ ->
            CliMonad.todo "encoder for enum not implemented"

        Bytes ->
            CliMonad.todo "encoder for bytes not implemented"

        Unit ->
            CliMonad.succeed Gen.Json.Encode.null


schemaToDecoder : Json.Schema.Definitions.Schema -> CliMonad Elm.Expression
schemaToDecoder schema =
    schemaToType schema
        |> CliMonad.andThen typeToDecoder


typeToDecoder : Type -> CliMonad Elm.Expression
typeToDecoder type_ =
    case type_ of
        Object properties ->
            let
                propertiesList : List ( String, Type )
                propertiesList =
                    Dict.toList properties
            in
            List.foldl
                (\( key, valueType ) prevExprRes ->
                    CliMonad.map2
                        (\internalDecoder prevExpr ->
                            Elm.Op.pipe
                                (Elm.apply
                                    Gen.Json.Decode.Extra.values_.andMap
                                    [ Gen.Json.Decode.field key internalDecoder ]
                                )
                                prevExpr
                        )
                        (typeToDecoder valueType)
                        prevExprRes
                )
                (CliMonad.succeed
                    (Gen.Json.Decode.succeed
                        (Elm.function
                            (List.map (\( key, _ ) -> ( toValueName key, Nothing )) propertiesList)
                            (\args ->
                                Elm.record
                                    (List.map2
                                        (\( key, _ ) arg -> ( toValueName key, arg ))
                                        propertiesList
                                        args
                                    )
                            )
                        )
                    )
                )
                propertiesList

        String ->
            CliMonad.succeed Gen.Json.Decode.string

        Int ->
            CliMonad.succeed Gen.Json.Decode.int

        Float ->
            CliMonad.succeed Gen.Json.Decode.float

        Bool ->
            CliMonad.succeed Gen.Json.Decode.bool

        Unit ->
            CliMonad.succeed (Gen.Json.Decode.succeed Elm.unit)

        List t ->
            CliMonad.map Gen.Json.Decode.list
                (typeToDecoder t)

        Value ->
            CliMonad.succeed Gen.Json.Decode.value

        Nullable t ->
            CliMonad.map
                (\decoder ->
                    Gen.Json.Decode.oneOf
                        [ Elm.apply
                            Gen.Json.Decode.values_.map
                            [ Elm.val "Present", decoder ]
                        , Gen.Json.Decode.null (Elm.val "Null")
                        ]
                )
                (typeToDecoder t)

        Named (TypeName name) ->
            CliMonad.succeed (Elm.val ("decode" ++ name))

        Enum _ ->
            CliMonad.todo "Enum decoder not implemented yet"

        Bytes ->
            CliMonad.todo "Bytes decoder not implemented yet"


responseToSchema : OpenApi.Response.Response -> CliMonad Json.Schema.Definitions.Schema
responseToSchema response =
    CliMonad.succeed response
        |> stepOrFail "Could not get application's application/json content"
            (OpenApi.Response.content
                >> Dict.get "application/json"
            )
        |> stepOrFail "The response's application/json content option doesn't have a schema"
            OpenApi.MediaType.schema
        |> CliMonad.map OpenApi.Schema.get


schemaToAnnotation : Json.Schema.Definitions.Schema -> CliMonad Elm.Annotation.Annotation
schemaToAnnotation schema =
    schemaToType schema |> CliMonad.andThen typeToAnnotation


typeToAnnotation : Type -> CliMonad Elm.Annotation.Annotation
typeToAnnotation type_ =
    case type_ of
        Nullable t ->
            typeToAnnotation t
                |> CliMonad.map
                    (\ann ->
                        Elm.Annotation.namedWith []
                            "Nullable"
                            [ ann ]
                    )

        Object fields ->
            Dict.toList fields
                |> CliMonad.combineMap (\( k, v ) -> CliMonad.map (Tuple.pair k) (typeToAnnotation v))
                |> CliMonad.map recordType

        String ->
            CliMonad.succeed Elm.Annotation.string

        Int ->
            CliMonad.succeed Elm.Annotation.int

        Float ->
            CliMonad.succeed Elm.Annotation.float

        Bool ->
            CliMonad.succeed Elm.Annotation.bool

        List t ->
            CliMonad.map Elm.Annotation.list (typeToAnnotation t)

        Enum anyOf ->
            CliMonad.map2
                (\intWord anns ->
                    let
                        (TypeName typeName) =
                            typifyName ("enum_" ++ intWord)
                    in
                    Elm.Annotation.namedWith [] typeName anns
                )
                (CliMonad.fromResult <| intToWord (List.length anyOf))
                (CliMonad.combineMap typeToAnnotation anyOf)

        Value ->
            CliMonad.succeed Gen.Json.Encode.annotation_.value

        Named (TypeName name) ->
            CliMonad.succeed <| Elm.Annotation.named [] name

        Bytes ->
            CliMonad.succeed Gen.Bytes.annotation_.bytes

        Unit ->
            CliMonad.succeed Elm.Annotation.unit


schemaToType : Json.Schema.Definitions.Schema -> CliMonad Type
schemaToType schema =
    case schema of
        Json.Schema.Definitions.BooleanSchema bool ->
            CliMonad.todoWithDefault Value "Boolean schema"

        Json.Schema.Definitions.ObjectSchema subSchema ->
            let
                nullable : CliMonad Type -> CliMonad Type
                nullable =
                    CliMonad.map Nullable

                singleTypeToType singleType =
                    case singleType of
                        Json.Schema.Definitions.ObjectType ->
                            objectSchemaToType subSchema

                        Json.Schema.Definitions.StringType ->
                            CliMonad.succeed String

                        Json.Schema.Definitions.IntegerType ->
                            CliMonad.succeed Int

                        Json.Schema.Definitions.NumberType ->
                            CliMonad.succeed Float

                        Json.Schema.Definitions.BooleanType ->
                            CliMonad.succeed Bool

                        Json.Schema.Definitions.NullType ->
                            CliMonad.todoWithDefault Value "Null type annotation"

                        Json.Schema.Definitions.ArrayType ->
                            case subSchema.items of
                                Json.Schema.Definitions.NoItems ->
                                    CliMonad.fail "Found an array type but no items definition"

                                Json.Schema.Definitions.ArrayOfItems _ ->
                                    CliMonad.todoWithDefault Value "Array of items as item definition"

                                Json.Schema.Definitions.ItemDefinition itemSchema ->
                                    CliMonad.map List (schemaToType itemSchema)

                enumAnnotation : List Json.Schema.Definitions.Schema -> CliMonad Type
                enumAnnotation anyOf =
                    anyOf
                        |> CliMonad.combineMap schemaToType
                        |> CliMonad.map Enum
            in
            case subSchema.type_ of
                Json.Schema.Definitions.SingleType singleType ->
                    singleTypeToType singleType

                Json.Schema.Definitions.AnyType ->
                    case subSchema.ref of
                        Just ref ->
                            CliMonad.map Named <| schemaTypeRef ref

                        Nothing ->
                            case subSchema.anyOf of
                                Nothing ->
                                    case subSchema.allOf of
                                        Just [ onlySchema ] ->
                                            schemaToType onlySchema

                                        Just [] ->
                                            CliMonad.succeed Value

                                        Just _ ->
                                            -- If we have more than one item in `allOf`, then it's _probably_ an object
                                            -- TODO: improve this to actually check if all the `allOf` subschema are objects.
                                            objectSchemaToType subSchema

                                        Nothing ->
                                            CliMonad.succeed Value

                                Just anyOf ->
                                    case anyOf of
                                        [ onlySchema ] ->
                                            schemaToType onlySchema

                                        [ firstSchema, secondSchema ] ->
                                            case ( firstSchema, secondSchema ) of
                                                ( Json.Schema.Definitions.ObjectSchema firstSubSchema, Json.Schema.Definitions.ObjectSchema secondSubSchema ) ->
                                                    -- The first 2 cases here are for pseudo-nullable schemas where the higher level schema type is AnyOf
                                                    -- but it's actually made up of only 2 types and 1 of them is nullable. This acts as a hack of sorts to
                                                    -- mark a value as nullable in the schema.
                                                    case ( firstSubSchema.type_, secondSubSchema.type_ ) of
                                                        ( Json.Schema.Definitions.SingleType Json.Schema.Definitions.NullType, _ ) ->
                                                            nullable (schemaToType secondSchema)

                                                        ( _, Json.Schema.Definitions.SingleType Json.Schema.Definitions.NullType ) ->
                                                            nullable (schemaToType firstSchema)

                                                        _ ->
                                                            enumAnnotation anyOf

                                                _ ->
                                                    enumAnnotation anyOf

                                        _ ->
                                            enumAnnotation anyOf

                Json.Schema.Definitions.NullableType singleType ->
                    nullable (singleTypeToType singleType)

                Json.Schema.Definitions.UnionType singleTypes ->
                    CliMonad.todoWithDefault Value "union type"


objectSchemaToType : Json.Schema.Definitions.SubSchema -> CliMonad Type
objectSchemaToType subSchema =
    let
        propertiesFromSchema : Json.Schema.Definitions.SubSchema -> CliMonad (Dict String Type)
        propertiesFromSchema sch =
            sch.properties
                |> Maybe.map (\(Json.Schema.Definitions.Schemata schemata) -> schemata)
                |> Maybe.withDefault []
                |> CliMonad.combineMap
                    (\( key, valueSchema ) ->
                        schemaToType valueSchema
                            |> CliMonad.withPath key
                            |> CliMonad.map (\ann -> ( key, ann ))
                    )
                |> CliMonad.map Dict.fromList

        schemaToProperties : Json.Schema.Definitions.Schema -> CliMonad (Dict String Type)
        schemaToProperties allOfItem =
            case allOfItem of
                Json.Schema.Definitions.ObjectSchema allOfItemSchema ->
                    CliMonad.map2 Dict.union
                        (propertiesFromSchema allOfItemSchema)
                        (propertiesFromRef allOfItemSchema)

                Json.Schema.Definitions.BooleanSchema _ ->
                    CliMonad.todoWithDefault Dict.empty "Boolean schema inside allOf"

        propertiesFromRef : Json.Schema.Definitions.SubSchema -> CliMonad (Dict String Type)
        propertiesFromRef allOfItem =
            case allOfItem.ref of
                Nothing ->
                    CliMonad.succeed Dict.empty

                Just ref ->
                    case String.split "/" ref of
                        [ "#", "components", "schemas", refName ] ->
                            CliMonad.fromApiSpec OpenApi.components
                                |> CliMonad.andThen
                                    (\maybeComponents ->
                                        case maybeComponents of
                                            Nothing ->
                                                CliMonad.fail <| "Could not find components in the schema, while looking up" ++ ref

                                            Just components ->
                                                case Dict.get refName (OpenApi.Components.schemas components) of
                                                    Nothing ->
                                                        CliMonad.fail <| "Could not find component's schema, while looking up" ++ ref

                                                    Just found ->
                                                        found
                                                            |> OpenApi.Schema.get
                                                            |> schemaToProperties
                                                            |> CliMonad.withPath refName
                                    )

                        _ ->
                            CliMonad.fail <| "Cannot understand reference " ++ ref

        propertiesFromAllOf =
            subSchema.allOf
                |> Maybe.withDefault []
                |> CliMonad.combineMap schemaToProperties
                |> CliMonad.map (List.foldl Dict.union Dict.empty)
    in
    CliMonad.map2
        (\schemaProps allOfProps ->
            allOfProps
                |> Dict.union schemaProps
                |> Object
        )
        (propertiesFromSchema subSchema)
        propertiesFromAllOf


makeNamespaceValid : String -> String
makeNamespaceValid str =
    String.map
        (\char ->
            if List.member char invalidModuleNameChars then
                '_'

            else
                char
        )
        str


removeInvalidChars : String -> String
removeInvalidChars str =
    String.filter (\char -> char /= '\'') str


invalidModuleNameChars : List Char
invalidModuleNameChars =
    [ ' '
    , '.'
    , '/'
    , '{'
    , '}'
    , '-'
    ]


intToWord : Int -> Result String String
intToWord i =
    case i of
        1 ->
            Ok "one"

        2 ->
            Ok "two"

        3 ->
            Ok "three"

        4 ->
            Ok "four"

        5 ->
            Ok "five"

        6 ->
            Ok "six"

        7 ->
            Ok "seven"

        8 ->
            Ok "eight"

        9 ->
            Ok "nine"

        10 ->
            Ok "ten"

        11 ->
            Ok "eleven"

        12 ->
            Ok "twelve"

        13 ->
            Ok "thirteen"

        14 ->
            Ok "fourteen"

        15 ->
            Ok "fifteen"

        16 ->
            Ok "sixteen"

        17 ->
            Ok "seventeen"

        18 ->
            Ok "eighteen"

        19 ->
            Ok "nineteen"

        _ ->
            if i < 0 then
                Err "Negative numbers aren't supported"

            else if i == 0 then
                Err "Zero isn't supported"

            else if i == 20 then
                Ok "twenty"

            else if i < 30 then
                intToWord (i - 20)
                    |> Result.map (\ones -> "twenty-" ++ ones)

            else if i == 30 then
                Ok "thirty"

            else if i < 40 then
                intToWord (i - 30)
                    |> Result.map (\ones -> "thirty-" ++ ones)

            else if i == 40 then
                Ok "forty"

            else if i < 50 then
                intToWord (i - 40)
                    |> Result.map (\ones -> "forty-" ++ ones)

            else if i == 50 then
                Ok "fifty"

            else if i < 60 then
                intToWord (i - 50)
                    |> Result.map (\ones -> "fifty-" ++ ones)

            else if i == 60 then
                Ok "sixty"

            else if i < 70 then
                intToWord (i - 60)
                    |> Result.map (\ones -> "sixty-" ++ ones)

            else if i == 70 then
                Ok "seventy"

            else if i < 80 then
                intToWord (i - 70)
                    |> Result.map (\ones -> "seventy-" ++ ones)

            else if i == 80 then
                Ok "eighty"

            else if i < 90 then
                intToWord (i - 80)
                    |> Result.map (\ones -> "eighty-" ++ ones)

            else if i == 90 then
                Ok "ninety"

            else if i < 100 then
                intToWord (i - 90)
                    |> Result.map (\ones -> "ninety-" ++ ones)

            else
                Err ("Numbers larger than 99 aren't currently supported and I got an " ++ String.fromInt i)
