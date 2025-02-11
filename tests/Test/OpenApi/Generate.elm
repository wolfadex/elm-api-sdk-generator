module Test.OpenApi.Generate exposing (suite)

import Char
import CliMonad
import Dict
import Elm
import Elm.Parser
import Elm.Syntax.Node
import Elm.ToString
import ElmSyntaxPrint
import Expect
import FastDict
import FastSet
import Fuzz
import Json.Decode
import Json.Encode
import List.Extra
import OpenApi
import OpenApi.Config
import OpenApi.Generate
import Parser
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
        -- Elm.ToString generates extremely over-indented code, so use the
        -- elm-syntax-format package to re-indent the code for human
        -- readability in "snapshot"-style tests.
        |> Maybe.map
            (\declaration ->
                let
                    srcFile : String
                    srcFile =
                        "module Foo exposing (..)\n\n\n" ++ declaration.body
                in
                case Elm.Parser.parseToFile srcFile of
                    Err errors ->
                        { declaration | body = Parser.deadEndsToString errors }

                    Ok file ->
                        List.head file.declarations
                            |> Maybe.map
                                (\decl ->
                                    { declaration
                                        | body =
                                            ElmSyntaxPrint.declaration
                                                { portDocumentationComment = Nothing, comments = [] }
                                                (Elm.Syntax.Node.value decl)
                                                { indent = 0 }
                                    }
                                )
                            |> Maybe.withDefault
                                { declaration
                                    | body = "Bad parse result in test for " ++ name ++ ": no declarations"
                                }
            )


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
                Expect.equal actualBody (unindent expectedBody)
            )
        |> Maybe.withDefault
            (Expect.fail
                ("expectType with " ++ type_ ++ " generated no declarations")
            )


{-| Wraps Test.test to make it easy to test files produced by a given OAS.
-}
testOas : String -> String -> ({ helperFile : GeneratedModule, jsonFile : GeneratedModule, typesFile : GeneratedModule } -> Expect.Expectation) -> Test.Test
testOas name oasSpec test =
    let
        namespace : List String
        namespace =
            Utils.sanitizeModuleName name
                |> Maybe.withDefault "BadOasNamespace"
                |> List.singleton
    in
    Test.test (name ++ " OAS") <|
        \() ->
            Json.Decode.decodeString OpenApi.decode oasSpec
                |> Result.mapError (\err -> "Failed to decode " ++ name ++ " OAS. Details: " ++ Json.Decode.errorToString err)
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
                            |> Result.mapError (\err -> "Failed to generate " ++ name ++ " OAS. Details: " ++ err.message ++ "\nPath: " ++ String.join "." err.path)
                    )
                |> (\result ->
                        case result of
                            Ok ( files, _ ) ->
                                case files of
                                    [ jsonFile, typesFile, helperFile ] ->
                                        test { jsonFile = jsonFile, typesFile = typesFile, helperFile = helperFile }

                                    [] ->
                                        Expect.fail (name ++ ": Expected to generate 3 files but found none")

                                    _ ->
                                        Expect.fail
                                            ([ name
                                             , ": Expected to generate 3 files but found "
                                             , List.length files |> String.fromInt
                                             , ": "
                                             , moduleNames files
                                             ]
                                                |> String.concat
                                            )

                            Err err ->
                                Expect.fail err
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

                            genFiles :
                                Result
                                    CliMonad.Message
                                    ( List { moduleName : List String, declarations : FastDict.Dict String { group : String, declaration : Elm.Declaration } }
                                    , { warnings : List CliMonad.Message, requiredPackages : FastSet.Set String }
                                    )
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
        , testOas "Additional Properties" additionalPropertiesOasString <|
            \{ jsonFile, typesFile } ->
                composeExpectations
                    [ expectDeclarationBody "StringLists"
                        typesFile
                        """
                                type alias StringLists =
                                    Dict.Dict String (List String)
                        """
                    , expectDeclarationBody "decodeStringLists"
                        jsonFile
                        """
                                decodeStringLists : Json.Decode.Decoder AdditionalProperties.Types.StringLists
                                decodeStringLists =
                                    Json.Decode.dict (Json.Decode.list Json.Decode.string)
                        """
                    , expectDeclarationBody "encodeStringLists"
                        jsonFile
                        """
                                encodeStringLists : AdditionalProperties.Types.StringLists -> Json.Encode.Value
                                encodeStringLists =
                                    Json.Encode.dict Basics.identity (Json.Encode.list Json.Encode.string)
                        """
                    , expectDeclarationBody "VagueExtras"
                        typesFile
                        """
                                type alias VagueExtras =
                                    { additionalProperties : Dict.Dict String Json.Encode.Value
                                    , a : Maybe String
                                    , b : String
                                    }
                        """
                    , expectDeclarationBody "encodeVagueExtras"
                        jsonFile
                        """
encodeVagueExtras : AdditionalProperties.Types.VagueExtras -> Json.Encode.Value
encodeVagueExtras rec =
    Json.Encode.object
        (List.append
            (List.filterMap
                Basics.identity
                [ Maybe.map
                    (\\mapUnpack -> ( "a", Json.Encode.string mapUnpack ))
                    rec.a
                , Just ( "b", Json.Encode.string rec.b )
                ]
            )
            (List.map
                (\\( key, value ) -> ( key, Basics.identity value ))
                (Dict.toList rec.additionalProperties)
            )
        )
                        """
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
                    , expectDeclarationBody "decodePopularity" jsonFile expectedDecodePopularity
                    , expectDeclarationBody "encodePopularity" jsonFile expectedEncodePopularity
                    ]
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
        "type": "object",
        "required": [
          "tags"
        ],
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
        }
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
      "VagueExtras": {
        "type": "object",
        "required": ["b"],
        "additionalProperties": true,
        "properties": {
          "a": {
            "type": "string"
          },
          "b": {
            "type": "string"
          }
        }
      }
    }
  }
}"""


expectedDecodePopularity : String
expectedDecodePopularity =
    """
decodePopularity : Json.Decode.Decoder AdditionalProperties.Types.Popularity
decodePopularity =
    Json.Decode.succeed
        (\\tags -> { tags = tags })
        |> OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field
                "tags"
                (Json.Decode.succeed
                    (\\declaredProperty additionalProperties ->
                        { declaredProperty =
                            declaredProperty
                        , additionalProperties =
                            additionalProperties
                        }
                    )
                    |> OpenApi.Common.jsonDecodeAndMap
                        (Json.Decode.field
                            "declaredProperty"
                            Json.Decode.string
                        )
                    |> OpenApi.Common.jsonDecodeAndMap
                        (Json.Decode.keyValuePairs
                            Json.Decode.value
                            |> Json.Decode.andThen
                                (\\keyValuePairs ->
                                    List.filterMap
                                        (\\( key, jsonValue ) ->
                                            if
                                                List.member
                                                    key
                                                    [ "declaredProperty"
                                                    ]
                                            then
                                                Nothing

                                            else
                                                let
                                                    additionalPropertyDecoder =
                                                        Json.Decode.succeed
                                                            (\\isPopular name ->
                                                                { isPopular =
                                                                    isPopular
                                                                , name =
                                                                    name
                                                                }
                                                            )
                                                            |> OpenApi.Common.jsonDecodeAndMap
                                                                (OpenApi.Common.decodeOptionalField
                                                                    "isPopular"
                                                                    Json.Decode.bool
                                                                )
                                                            |> OpenApi.Common.jsonDecodeAndMap
                                                                (Json.Decode.field
                                                                    "name"
                                                                    Json.Decode.string
                                                                )
                                                in
                                                case
                                                    Json.Decode.decodeValue
                                                        additionalPropertyDecoder
                                                        jsonValue
                                                of
                                                    Ok decodedValue ->
                                                        Just
                                                            (Result.Ok
                                                                ( key
                                                                , decodedValue
                                                                )
                                                            )

                                                    Err decodeError ->
                                                        Just
                                                            (Result.Err
                                                                ("Field '"
                                                                    ++ key
                                                                    ++ "': "
                                                                    ++ Json.Decode.errorToString
                                                                        decodeError
                                                                )
                                                            )
                                        )
                                        keyValuePairs
                                        |> (\\resultPairs ->
                                                let
                                                    fieldErrors =
                                                        List.filterMap
                                                            (\\field ->
                                                                case field of
                                                                    Ok _ ->
                                                                        Nothing

                                                                    Err error ->
                                                                        Just
                                                                            error
                                                            )
                                                            resultPairs
                                                in
                                                if
                                                    List.isEmpty
                                                        fieldErrors
                                                then
                                                    resultPairs
                                                        |> List.filterMap
                                                            Result.toMaybe
                                                        |> Dict.fromList
                                                        |> Json.Decode.succeed

                                                else
                                                    [ \"\"\"Errors while decoding additionalProperties:
- \"\"\"
                                                    , String.join
                                                        \"\"\"

- \"\"\"
                                                        fieldErrors
                                                    , \"\"\"
\"\"\"
                                                    ]
                                                        |> String.concat
                                                        |> Json.Decode.fail
                                           )
                                )
                        )
                )
            )
                                """


expectedEncodePopularity : String
expectedEncodePopularity =
    """
encodePopularity : AdditionalProperties.Types.Popularity -> Json.Encode.Value
encodePopularity rec =
    Json.Encode.object
        [ ( "tags"
          , Json.Encode.object
                (List.append
                    [ ( "declaredProperty"
                      , Json.Encode.string rec.tags.declaredProperty
                      )
                    ]
                    (List.map
                        (\\( key, value ) ->
                            ( key
                            , Json.Encode.object
                                (List.filterMap
                                    Basics.identity
                                    [ Maybe.map
                                        (\\mapUnpack ->
                                            ( "isPopular"
                                            , Json.Encode.bool mapUnpack
                                            )
                                        )
                                        value.isPopular
                                    , Just
                                        ( "name"
                                        , Json.Encode.string value.name
                                        )
                                    ]
                                )
                            )
                        )
                        (Dict.toList rec.tags.additionalProperties)
                    )
                )
          )
        ]
"""
