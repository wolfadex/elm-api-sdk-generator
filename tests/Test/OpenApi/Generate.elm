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
import OpenApi
import OpenApi.Config
import OpenApi.Generate
import Result.Extra
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


{-| Remove all indentation and newlines from a stringified Elm declaration
-}
toOneLine : String -> String
toOneLine declaration =
    declaration
        |> String.split "\n"
        |> List.map String.trim
        |> String.join " "
        |> String.trim


{-| Expect the `type_` from `module_` to be stringified as `expectedBody`

expectedBody may have mutiple lines, with arbitrary indentation (leading and
trailing spaces and newlines will not be considered in the comparison).

-}
expectType : GeneratedModule -> String -> String -> Expect.Expectation
expectType module_ type_ expectedBody =
    module_.declarations
        |> FastDict.get type_
        |> Maybe.map (.declaration >> Elm.ToString.declaration)
        |> Maybe.andThen List.head
        |> Maybe.map .body
        |> Maybe.map
            (\actualBody ->
                Expect.equal (toOneLine actualBody) (toOneLine expectedBody)
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
                            [ apiFile, jsonFile, typesFile, helperFile ] ->
                                composeExpectations
                                    [ expectModuleName apiFile (namespace ++ [ "Api" ])
                                    , expectModuleName typesFile (namespace ++ [ "Types" ])
                                    , expectModuleName jsonFile (namespace ++ [ "Json" ])
                                    , expectModuleName helperFile [ "OpenApi", "Common" ]
                                    , expectType typesFile
                                        "Popularity"
                                        """
                                            type alias Popularity =
                                                { tags :
                                                    { additionalProperties : Maybe (Dict.Dict String { isPopular : Maybe Bool, name : String })
                                                    , declaredProperty : String
                                                    }
                                                }
                                        """
                                    , expectType typesFile
                                        "StringLists"
                                        "type alias StringLists = Dict.Dict String (List String)"
                                    , expectType typesFile
                                        "VagueExtras"
                                        """
                                            type alias VagueExtras =
                                                { additionalProperties : Maybe (Dict.Dict String Json.Encode.Value)
                                                , declaredProperty : Maybe String
                                                }
                                        """
                                    ]

                            [] ->
                                Expect.fail "Expected to generate 4 files but found none"

                            _ ->
                                Expect.fail
                                    ("Expected to generate 4 files but found "
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
        "additionalProperties": {
          "items": {
            "type": "string"
          },
          "type": "array"
        },
        "type": "object"
      },
      "VagueExtras": {
        "additionalProperties": true,
        "properties": {
          "declaredProperty": {
            "type": "string"
          }
        },
        "type": "object"
      }
    }
  },
  "paths": {
    "/api/list": {
      "description": null,
      "get": {
        "operationId": "getStringLists",
        "responses": {
          "200": {
            "$ref": "#/components/schemas/StringLists"
          }
        }
      },
      "summary": "Get StringLists"
    },
    "/api/popularity": {
      "description": null,
      "get": {
        "operationId": "getPopularity",
        "responses": {
          "200": {
            "$ref": "#/components/schemas/Popularity"
          }
        }
      },
      "summary": "Get Popularity"
    }
  }
}"""
