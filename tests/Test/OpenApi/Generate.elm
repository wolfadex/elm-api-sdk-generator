module Test.OpenApi.Generate exposing (suite)

import Char
import CliMonad
import Elm
import Expect
import FastDict
import FastSet
import Fuzz
import Json.Decode
import OpenApi
import OpenApi.Config
import OpenApi.Generate
import String.Extra
import Test exposing (Test)
import Utils


moduleNames : List { a | moduleName : List String } -> String
moduleNames modules =
    modules
        |> List.map (\{ moduleName } -> String.join "." moduleName)
        |> String.Extra.toSentenceOxford


expectModuleName : { a | moduleName : List String } -> List String -> Expect.Expectation
expectModuleName file expectedPath =
    Expect.equal file.moduleName expectedPath
        |> Expect.onFail
            ("Expected to generate a file with the module name "
                ++ String.join "." expectedPath
                ++ " but I found "
                ++ moduleNames [ file ]
            )


composeExpectations : List Expect.Expectation -> Expect.Expectation
composeExpectations expectations =
    Expect.all
        (List.map (\x -> always x) expectations)
        ()


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
                                    ( List
                                        { moduleName : List String
                                        , declarations : FastDict.Dict String { group : String, declaration : Elm.Declaration }
                                        }
                                    , { warnings : List CliMonad.Message
                                      , requiredPackages : FastSet.Set String
                                      }
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
        ]
