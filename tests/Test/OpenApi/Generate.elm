module Test.OpenApi.Generate exposing (suite)

import Char
import CliMonad
import Dict
import Elm
import Elm.ToString
import Expect
import FastDict
import FastSet
import Fuzz
import Json.Decode
import List.Extra
import OpenApi
import OpenApi.Config
import OpenApi.Generate
import String.Extra
import Test exposing (Test)
import Utils


type alias GeneratedModule =
    { moduleName : List String
    , declarations : FastDict.Dict String { group : String, declaration : Elm.Declaration }
    }


moduleNames : List { a | moduleName : List String } -> String
moduleNames modules =
    modules
        |> List.map (.moduleName >> String.join ".")
        |> String.Extra.toSentenceOxford


expectModuleName : { a | moduleName : List String } -> List String -> Expect.Expectation
expectModuleName file expectedPath =
    Expect.equal file.moduleName expectedPath
        |> Expect.onFail ("Expected to generate a file with the module name " ++ String.join "." expectedPath ++ " but I found " ++ moduleNames [ file ])


composeExpectations : List Expect.Expectation -> Expect.Expectation
composeExpectations expectations =
    Expect.all
        (List.map (\x -> always x) expectations)
        ()


getIndentation : String -> Int
getIndentation text =
    text
        |> String.lines
        |> List.Extra.dropWhile String.isEmpty
        |> List.head
        |> Maybe.map (\firstLine -> String.length firstLine - String.length (String.trimLeft firstLine))
        |> Maybe.withDefault 0


{-| Remove all indentation and newlines from a stringified Elm declaration
-}
unindent : String -> String
unindent declaration =
    let
        indentation : Int
        indentation =
            getIndentation declaration
    in
    declaration
        |> String.lines
        |> List.Extra.dropWhile String.isEmpty
        |> List.map (\line -> String.dropLeft indentation line)
        |> String.join "\n"
        |> String.trim


getDeclaration : String -> GeneratedModule -> Maybe { imports : String, docs : String, signature : String, body : String }
getDeclaration name module_ =
    module_.declarations
        |> FastDict.get name
        |> Maybe.map (.declaration >> Elm.ToString.declaration)
        |> Maybe.andThen List.head


{-| Expect the `type_` from `module_` to be stringified as `expectedBody`

expectedBody may have mutiple lines, with arbitrary indentation (leading and
trailing spaces and newlines will not be considered in the comparison).

-}
expectDeclarationBody : String -> GeneratedModule -> String -> Expect.Expectation
expectDeclarationBody type_ module_ expectedBody =
    getDeclaration type_ module_
        |> Maybe.map .body
        |> Maybe.map
            (\actualBody ->
                let
                    _ =
                        Debug.log actualBody "<- actualBody\n"
                in
                Expect.equal (String.trim actualBody) (unindent expectedBody)
            )
        |> Maybe.withDefault
            (Expect.fail
                ("expectType with " ++ type_ ++ " generated no declarations")
            )


suite : Test
suite =
    Test.describe "open api generation"
        [ Test.fuzz Fuzz.string
            "sanitizeModuleName"
          <|
            \inputName ->
                let
                    moduleName : Maybe String
                    moduleName =
                        Utils.sanitizeModuleName inputName
                in
                if String.any Char.isAlphaNum inputName then
                    case moduleName of
                        Just finalName ->
                            if String.all Char.isAlphaNum finalName then
                                Expect.pass

                            else
                                Expect.fail ("Unexpected chars in module name: " ++ finalName)

                        Nothing ->
                            Expect.fail "Unexpected Nothing"

                else
                    Expect.equal moduleName Nothing
        , Test.fuzz Fuzz.string
            "valid output file & module name"
          <|
            \inputName ->
                let
                    oasString : String
                    oasString =
                        """{
  "openapi": "3.1.0",
  "info": {
    "title":    \"""" ++ inputName ++ """",
    "version": "1.0.0"
  },
  "paths": {
    "/": {
      "get": {
      }
    }
  }
}"""
                in
                case Json.Decode.decodeString OpenApi.decode oasString of
                    Err _ ->
                        Expect.pass

                    Ok oas ->
                        let
                            namespace : List String
                            namespace =
                                Utils.sanitizeModuleName inputName
                                    |> Maybe.withDefault "Carl"
                                    |> List.singleton

                            genFiles : Result CliMonad.Message ( List { moduleName : List String, declarations : FastDict.Dict String { group : String, declaration : Elm.Declaration } }, { warnings : List CliMonad.Message, requiredPackages : FastSet.Set String } )
                            genFiles =
                                OpenApi.Generate.files
                                    { namespace = namespace
                                    , generateTodos = False
                                    , effectTypes = [ OpenApi.Config.ElmHttpCmd, OpenApi.Config.ElmHttpTask ]
                                    , server = OpenApi.Config.Default
                                    , formats = OpenApi.Config.defaultFormats
                                    }
                                    oas
                        in
                        case genFiles of
                            Err _ ->
                                Expect.fail "Unexpected generation error"

                            Ok ( files, _ ) ->
                                case files of
                                    [ jsonFile, apiFile, helperFile ] ->
                                        let
                                            jsonPath : List String
                                            jsonPath =
                                                namespace ++ [ "Json" ]

                                            apiPath : List String
                                            apiPath =
                                                namespace ++ [ "Api" ]

                                            helperPath : List String
                                            helperPath =
                                                namespace ++ [ "OpenApi" ]
                                        in
                                        composeExpectations
                                            [ expectModuleName jsonFile jsonPath
                                            , expectModuleName apiFile apiPath
                                            , expectModuleName helperFile helperPath
                                            ]

                                    [] ->
                                        Expect.fail "Expected to generate 3 files but found none"

                                    _ ->
                                        Expect.fail
                                            ("Expected to generate 3 files but found "
                                                ++ (List.length files |> String.fromInt)
                                                ++ ": "
                                                ++ moduleNames files
                                            )

        -- Known bug: https://github.com/wolfadex/elm-open-api-cli/issues/48
        , Test.test "The OAS title: service API (params in:body)" <|
            \() ->
                let
                    moduleName : Maybe String
                    moduleName =
                        Utils.sanitizeModuleName "service API (params in:body)"
                in
                Expect.equal moduleName (Just "ServiceApiParamsInBody")
        , Test.test "Additional Properties" <|
            \() ->
                let
                    namespace : List String
                    namespace =
                        Utils.sanitizeModuleName "Additional Properties"
                            |> Maybe.withDefault "Earl"
                            |> List.singleton

                    genFiles : Result CliMonad.Message ( List { moduleName : List String, declarations : FastDict.Dict String { group : String, declaration : Elm.Declaration } }, { warnings : List CliMonad.Message, requiredPackages : FastSet.Set String } )
                    genFiles =
                        Json.Decode.decodeString OpenApi.decode additionalPropertiesOasString
                            |> Result.mapError (\_ -> { message = "Failed to decode additionalProperties schema", path = [] })
                            |> Result.andThen
                                (\oas ->
                                    OpenApi.Generate.files
                                        { namespace = namespace
                                        , generateTodos = False
                                        , effectTypes = [ OpenApi.Config.ElmHttpTask ]
                                        , server = OpenApi.Config.Default
                                        , formats = OpenApi.Config.defaultFormats
                                        }
                                        oas
                                )
                in
                case genFiles of
                    Err { message } ->
                        Expect.fail message

                    Ok ( files, _ ) ->
                        case files of
                            [ jsonFile, typesFile, helperFile ] ->
                                composeExpectations
                                    [ expectModuleName typesFile (namespace ++ [ "Types" ])
                                    , expectModuleName jsonFile (namespace ++ [ "Json" ])
                                    , expectModuleName helperFile [ "OpenApi", "Common" ]
                                    , expectDeclarationBody "Popularity"
                                        typesFile
                                        """
                                            type alias Popularity =
                                                { tags :
                                                    { additionalProperties :
                                                        Dict.Dict String { isPopular : Maybe Bool, name : String }
                                                    , declaredProperty : String
                                                    }
                                                }
                                        """
                                    , expectDeclarationBody "StringLists"
                                        typesFile
                                        """
                                            type alias StringLists =
                                                Dict.Dict String (List String)
                                        """
                                    , expectDeclarationBody "OnlyExtras"
                                        typesFile
                                        """
                                            type alias OnlyExtras =
                                                Dict.Dict String String
                                        """
                                    , expectDeclarationBody "decodeOnlyExtras"
                                        jsonFile
                                        """
                                            decodeOnlyExtras : Json.Decode.Decoder AdditionalProperties.Types.OnlyExtras
                                            decodeOnlyExtras =
                                                Json.Decode.dict Json.Decode.string
                                        """
                                    , expectDeclarationBody "encodeOnlyExtras"
                                        jsonFile
                                        """
                                            encodeOnlyExtras : AdditionalProperties.Types.OnlyExtras -> Json.Encode.Value
                                            encodeOnlyExtras =
                                                Json.Encode.dict Basics.identity Json.Encode.string
                                        """
                                    , expectDeclarationBody "VagueExtras"
                                        typesFile
                                        """
                                            type alias VagueExtras =
                                                { additionalProperties : Dict.Dict String Json.Encode.Value
                                                , declaredProperty : Maybe String
                                                }
                                        """
                                    , expectDeclarationBody "decodeVagueExtras"
                                        jsonFile
                                        """
                                            decodeVagueExtras : Json.Decode.Decoder AdditionalProperties.Types.VagueExtras
                                            decodeVagueExtras =
                                                Json.Decode.succeed
                                                    (\\declaredProperty additionalProperties ->
                                                        { declaredProperty = declaredProperty
                                                        , additionalProperties = additionalProperties
                                                        }
                                                    ) |> OpenApi.Common.jsonDecodeAndMap
                                                        (OpenApi.Common.decodeOptionalField
                                                                "declaredProperty"
                                                                Json.Decode.string
                                                        )
                                        """
                                    ]

                            [] ->
                                Expect.fail "Expected to generate 4 files but found none"

                            _ ->
                                Expect.fail
                                    ("Expected to generate 3 files but found "
                                        ++ (List.length files |> String.fromInt)
                                        ++ ": "
                                        ++ moduleNames files
                                    )
        , Test.test "Decoder" <|
            \() ->
                let
                    json : String
                    json =
                        """
                        {
                          "tags": {
                            "declaredProperty": "declared",
                            "foo": { "isPopular": true, "name": "foo" },
                            "bar": { "name": "bar" }
                          }
                        }"""

                    jsonDecodeAndMap :
                        Json.Decode.Decoder a
                        -> Json.Decode.Decoder (a -> value)
                        -> Json.Decode.Decoder value
                    jsonDecodeAndMap =
                        Json.Decode.map2 (|>)

                    decodeOptionalField : String -> Json.Decode.Decoder t -> Json.Decode.Decoder (Maybe t)
                    decodeOptionalField key fieldDecoder =
                        Json.Decode.andThen
                            (\andThenUnpack ->
                                if andThenUnpack then
                                    Json.Decode.field
                                        key
                                        (Json.Decode.oneOf
                                            [ Json.Decode.map Just fieldDecoder
                                            , Json.Decode.null Nothing
                                            ]
                                        )

                                else
                                    Json.Decode.succeed Nothing
                            )
                            (Json.Decode.oneOf
                                [ Json.Decode.map
                                    (\_ -> True)
                                    (Json.Decode.field key Json.Decode.value)
                                , Json.Decode.succeed False
                                ]
                            )

                    decoder : Json.Decode.Decoder { tags : { additionalProperties : Dict.Dict String { isPopular : Maybe Bool, name : String }, declaredProperty : String } }
                    decoder =
                        Json.Decode.succeed
                            (\tags -> { tags = tags })
                            |> jsonDecodeAndMap
                                (Json.Decode.field "tags"
                                    (Json.Decode.succeed
                                        (\additionalProperties declaredProperty ->
                                            { additionalProperties =
                                                additionalProperties
                                            , declaredProperty =
                                                declaredProperty
                                            }
                                        )
                                        |> jsonDecodeAndMap
                                            -- Decode the additionalProperties first.
                                            (Json.Decode.keyValuePairs Json.Decode.value
                                                |> Json.Decode.andThen
                                                    (\keyValuePairs ->
                                                        keyValuePairs
                                                            |> List.filterMap
                                                                (\( k, v ) ->
                                                                    -- Skip keys of declared properties
                                                                    if List.member k [ "declaredProperty" ] then
                                                                        Nothing

                                                                    else
                                                                        let
                                                                            -- Decodes each additionalProperties value
                                                                            addPropValueDecoder : Json.Decode.Decoder { isPopular : Maybe Bool, name : String }
                                                                            addPropValueDecoder =
                                                                                Json.Decode.succeed
                                                                                    (\isPopular name ->
                                                                                        { isPopular = isPopular
                                                                                        , name = name
                                                                                        }
                                                                                    )
                                                                                    |> jsonDecodeAndMap (decodeOptionalField "isPopular" Json.Decode.bool)
                                                                                    |> jsonDecodeAndMap (Json.Decode.field "name" Json.Decode.string)
                                                                        in
                                                                        case Json.Decode.decodeValue addPropValueDecoder v of
                                                                            Err decodeError ->
                                                                                Just (Err ("Field '" ++ k ++ "': " ++ Json.Decode.errorToString decodeError))

                                                                            Ok value ->
                                                                                Just (Ok ( k, value ))
                                                                )
                                                            |> (\resultPairs ->
                                                                    let
                                                                        fieldErrors : List String
                                                                        fieldErrors =
                                                                            resultPairs
                                                                                |> List.filterMap
                                                                                    (\res ->
                                                                                        case res of
                                                                                            Ok _ ->
                                                                                                Nothing

                                                                                            Err error ->
                                                                                                Just error
                                                                                    )
                                                                    in
                                                                    if List.isEmpty fieldErrors then
                                                                        resultPairs
                                                                            |> List.filterMap Result.toMaybe
                                                                            |> Dict.fromList
                                                                            |> Json.Decode.succeed

                                                                    else
                                                                        [ "Encountered errors while decoding additionalProperties:\n- "
                                                                        , fieldErrors |> String.join "\n\n- "
                                                                        , "\n"
                                                                        ]
                                                                            |> String.concat
                                                                            |> Json.Decode.fail
                                                               )
                                                    )
                                            )
                                        -- Decode all the declared properties now
                                        |> jsonDecodeAndMap
                                            (Json.Decode.field "declaredProperty" Json.Decode.string)
                                    )
                                )
                in
                case Json.Decode.decodeString decoder json of
                    Err err ->
                        err
                            |> Json.Decode.errorToString
                            |> Expect.fail

                    Ok stuff ->
                        Expect.equal stuff
                            { tags =
                                { additionalProperties =
                                    Dict.fromList
                                        [ ( "bar", { isPopular = Nothing, name = "bar" } )
                                        , ( "foo", { isPopular = Just True, name = "foo" } )
                                        ]
                                , declaredProperty = "declared"
                                }
                            }
        ]


additionalPropertiesOasString : String
additionalPropertiesOasString =
    """
{
  "openapi": "3.0.1",
  "info": {
    "title": "Additional Properties",
    "version": "1"
  },
  "components": {
    "schemas": {
      "Popularity": {
        "properties": {
          "tags": {
            "additionalProperties": {
              "properties": {
                "isPopular": {
                  "type": "boolean"
                },
                "name": {
                  "type": "string"
                }
              },
              "required": [
                "name"
              ],
              "type": "object"
            },
            "properties": {
              "declaredProperty": {
                "type": "string"
              }
            },
            "required": [
              "declaredProperty"
            ],
            "type": "object"
          }
        },
        "required": [
          "tags"
        ],
        "type": "object"
      },
      "StringLists": {
        "type": "object",
        "additionalProperties": {
          "type": "array",
          "items": {
            "type": "string"
          }
        }
      },
      "OnlyExtras": {
        "type": "object",
        "additionalProperties": {
          "type": "string"
        }
      },
      "VagueExtras": {
        "type": "object",
        "additionalProperties": true,
        "properties": {
          "declaredProperty": {
            "type": "string"
          }
        }
      }
    }
  }
}"""
