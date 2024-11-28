module OpenApi.Config exposing
    ( Config, EffectType(..), Format, Input, Path(..), Server(..)
    , init, inputFrom, pathFromString
    , withAutoConvertSwagger, withEffectTypes, withFormat, withFormats, withGenerateTodos, withInput, withSwaggerConversionCommand, withSwaggerConversionUrl
    , withOutputModuleName, withOverrides, withServer, withWriteMergedTo
    , autoConvertSwagger, inputs, outputDirectory, swaggerConversionCommand, swaggerConversionUrl
    , oasPath, overrides, writeMergedTo
    , toGenerationConfig, pathToString
    , defaultFormats
    )

{-|


# Types

@docs Config, EffectType, Format, Input, Path, Server


# Creation

@docs init, inputFrom, pathFromString
@docs withAutoConvertSwagger, withEffectTypes, withFormat, withFormats, withGenerateTodos, withInput, withSwaggerConversionCommand, withSwaggerConversionUrl
@docs withOutputModuleName, withOverrides, withServer, withWriteMergedTo


# Config properties

@docs autoConvertSwagger, inputs, outputDirectory, swaggerConversionCommand, swaggerConversionUrl


# Input properties

@docs oasPath, overrides, writeMergedTo


# Output

@docs toGenerationConfig, pathToString


# Internal

@docs defaultFormats

-}

import Common
import Dict
import Elm
import Elm.Annotation
import Elm.Op
import Gen.Date
import Gen.Int64
import Gen.Json.Decode
import Gen.Json.Encode
import Gen.Maybe
import Gen.Parser.Advanced
import Gen.Result
import Gen.Rfc3339
import Gen.String
import Gen.Time
import Gen.Url
import OpenApi
import OpenApi.Info
import Url
import Utils


type Config
    = Config
        { inputs : List Input
        , outputDirectory : String
        , generateTodos : Bool
        , autoConvertSwagger : Bool
        , swaggerConversionUrl : String
        , swaggerConversionCommand : Maybe { command : String, args : List String }
        , staticFormats : List Format
        , dynamicFormats : List { format : String, basicType : Common.BasicType } -> List Format
        }


type Input
    = Input
        { oasPath : Path
        , outputModuleName : Maybe (List String)
        , server : Server
        , overrides : List Path
        , writeMergedTo : Maybe String
        , effectTypes : List EffectType
        }


type EffectType
    = ElmHttpCmd
    | ElmHttpCmdRecord
    | ElmHttpCmdRisky
    | ElmHttpTask
    | ElmHttpTaskRecord
    | ElmHttpTaskRisky
    | DillonkearnsElmPagesTask
    | DillonkearnsElmPagesTaskRecord
    | LamderaProgramTestCmd
    | LamderaProgramTestCmdRisky
    | LamderaProgramTestCmdRecord
    | LamderaProgramTestTask
    | LamderaProgramTestTaskRisky
    | LamderaProgramTestTaskRecord


type Server
    = Default
    | Single String
    | Multiple (Dict.Dict String String)


type alias Format =
    { basicType : Common.BasicType
    , format : String
    , encode : Elm.Expression -> Elm.Expression
    , decoder : Elm.Expression
    , toParamString : Elm.Expression -> Elm.Expression
    , annotation : Elm.Annotation.Annotation
    , sharedDeclarations : List ( String, Elm.Expression )
    , requiresPackages : List String
    }


type Path
    = File String -- swagger.json ./swagger.json /folder/swagger.json
    | Url Url.Url -- https://petstore3.swagger.io/api/v3/openapi.json


pathFromString : String -> Path
pathFromString path =
    case Url.fromString path of
        Just url ->
            Url url

        Nothing ->
            File path


pathToString : Path -> String
pathToString pathType =
    case pathType of
        File file ->
            file

        Url url ->
            Url.toString url


init : String -> Config
init initialOutputDirectory =
    { inputs = []
    , outputDirectory = initialOutputDirectory
    , generateTodos = False
    , autoConvertSwagger = False
    , swaggerConversionUrl = "https://converter.swagger.io/api/convert"
    , swaggerConversionCommand = Nothing
    , staticFormats = defaultFormats
    , dynamicFormats = \_ -> []
    }
        |> Config


inputFrom : Path -> Input
inputFrom path =
    { oasPath = path
    , outputModuleName = Nothing
    , server = Default
    , overrides = []
    , writeMergedTo = Nothing
    , effectTypes = [ ElmHttpCmd, ElmHttpTask ]
    }
        |> Input



-------------
-- Formats --
-------------


defaultFormats : List Format
defaultFormats =
    [ dateTimeFormat
    , dateFormat
    , uriFormat
    , int64Format
    ]


dateTimeFormat : Format
dateTimeFormat =
    let
        toString : Elm.Expression -> Elm.Expression
        toString instant =
            Gen.Rfc3339.make_.dateTimeOffset
                (Elm.record
                    [ ( "instant", instant )
                    , ( "offset"
                      , Elm.record
                            [ ( "hour", Elm.int 0 )
                            , ( "minute", Elm.int 0 )
                            ]
                      )
                    ]
                )
                |> Gen.Rfc3339.toString
    in
    { basicType = Common.String
    , format = "date-time"
    , annotation = Gen.Time.annotation_.posix
    , encode =
        \instant ->
            instant
                |> toString
                |> Gen.Json.Encode.call_.string
    , decoder =
        Gen.Json.Decode.string
            |> Gen.Json.Decode.andThen
                (\raw ->
                    Gen.Result.caseOf_.result
                        (Gen.Parser.Advanced.call_.run Gen.Rfc3339.dateTimeOffsetParser raw)
                        { ok = \record -> Gen.Json.Decode.succeed (Elm.get "instant" record)

                        -- TODO: improve error message
                        , err = \_ -> Gen.Json.Decode.fail "Invalid RFC-3339 date-time"
                        }
                )
    , toParamString = toString
    , sharedDeclarations = []
    , requiresPackages =
        [ "elm/parser"
        , "elm/time"
        , "justinmimbs/time-extra"
        , "wolfadex/elm-rfc3339"
        ]
    }


dateFormat : Format
dateFormat =
    { basicType = Common.String
    , format = "date"
    , annotation = Gen.Date.annotation_.date
    , encode =
        \date ->
            date
                |> Gen.Date.toIsoString
                |> Gen.Json.Encode.call_.string
    , toParamString = Gen.Date.toIsoString
    , decoder =
        Gen.Json.Decode.string
            |> Gen.Json.Decode.andThen
                (\raw ->
                    Gen.Result.caseOf_.result
                        (Gen.Date.call_.fromIsoString raw)
                        { ok = Gen.Json.Decode.succeed
                        , err = Gen.Json.Decode.call_.fail
                        }
                )
    , sharedDeclarations = []
    , requiresPackages = [ "justinmimbs/date" ]
    }


uriFormat : Format
uriFormat =
    { basicType = Common.String
    , format = "uri"
    , annotation = Gen.Url.annotation_.url
    , encode =
        \url ->
            url
                |> Gen.Url.toString
                |> Gen.Json.Encode.call_.string
    , toParamString = Gen.Url.toString
    , decoder =
        Gen.Json.Decode.string
            |> Gen.Json.Decode.andThen
                (\raw ->
                    Gen.Maybe.caseOf_.maybe
                        (Gen.Url.call_.fromString raw)
                        { just = Gen.Json.Decode.succeed
                        , nothing =
                            Gen.Json.Decode.call_.fail
                                (Elm.Op.append raw (Elm.string " is not a valid URL"))
                        }
                )
    , sharedDeclarations = []
    , requiresPackages = [ "justinmimbs/date" ]
    }


int64Format : Format
int64Format =
    { basicType = Common.Integer
    , format = "int64"
    , annotation = Gen.Int64.annotation_.int64
    , encode =
        \int64 ->
            int64
                |> Gen.Int64.toSignedString
                |> Gen.String.call_.toInt
                |> Gen.Maybe.withDefault (Elm.int 0)
                |> Gen.Json.Encode.call_.int
    , decoder = Gen.Json.Decode.int |> Gen.Json.Decode.map Gen.Int64.call_.fromInt
    , toParamString = Gen.Int64.toSignedString
    , sharedDeclarations = []
    , requiresPackages = [ "folkertdev/elm-int64" ]
    }



-------------
-- Setters --
-------------


withEffectTypes : List EffectType -> Input -> Input
withEffectTypes effectTypes (Input input) =
    Input { input | effectTypes = effectTypes }


withOutputModuleName : List String -> Input -> Input
withOutputModuleName moduleName (Input input) =
    Input { input | outputModuleName = Just moduleName }


withOverrides : List Path -> Input -> Input
withOverrides newOverrides (Input input) =
    Input { input | overrides = newOverrides }


withGenerateTodos : Bool -> Config -> Config
withGenerateTodos generateTodos (Config config) =
    Config { config | generateTodos = generateTodos }


withAutoConvertSwagger : Bool -> Config -> Config
withAutoConvertSwagger newAutoConvertSwagger (Config config) =
    Config { config | autoConvertSwagger = newAutoConvertSwagger }


withSwaggerConversionUrl : String -> Config -> Config
withSwaggerConversionUrl newSwaggerConversionUrl (Config config) =
    Config { config | swaggerConversionUrl = newSwaggerConversionUrl }


withSwaggerConversionCommand : { command : String, args : List String } -> Config -> Config
withSwaggerConversionCommand newSwaggerConversionCommand (Config config) =
    Config { config | swaggerConversionCommand = Just newSwaggerConversionCommand }


withServer : Server -> Input -> Input
withServer newServer (Input input) =
    Input { input | server = newServer }


withWriteMergedTo : String -> Input -> Input
withWriteMergedTo newWriteMergedTo (Input input) =
    Input { input | writeMergedTo = Just newWriteMergedTo }


withFormat : Format -> Config -> Config
withFormat newFormat (Config config) =
    Config { config | staticFormats = newFormat :: config.staticFormats }


withFormats :
    (List { format : String, basicType : Common.BasicType } -> List Format)
    -> Config
    -> Config
withFormats newFormat (Config config) =
    Config { config | dynamicFormats = \input -> newFormat input ++ config.dynamicFormats input }


withInput : Input -> Config -> Config
withInput input (Config config) =
    Config { config | inputs = input :: config.inputs }



-------------
-- Getters --
-------------


swaggerConversionUrl : Config -> String
swaggerConversionUrl (Config config) =
    config.swaggerConversionUrl


swaggerConversionCommand : Config -> Maybe { command : String, args : List String }
swaggerConversionCommand (Config config) =
    config.swaggerConversionCommand


autoConvertSwagger : Config -> Bool
autoConvertSwagger (Config config) =
    config.autoConvertSwagger


outputDirectory : Config -> String
outputDirectory (Config config) =
    config.outputDirectory


inputs : Config -> List Input
inputs (Config config) =
    List.reverse config.inputs


oasPath : Input -> Path
oasPath (Input input) =
    input.oasPath


writeMergedTo : Input -> Maybe String
writeMergedTo (Input input) =
    input.writeMergedTo


overrides : Input -> List Path
overrides (Input input) =
    input.overrides



------------
-- Output --
------------


toGenerationConfig :
    List { format : String, basicType : Common.BasicType }
    -> Config
    -> List ( Input, OpenApi.OpenApi )
    ->
        List
            ( { namespace : List String
              , generateTodos : Bool
              , effectTypes : List EffectType
              , server : Server
              , formats : List Format
              }
            , OpenApi.OpenApi
            )
toGenerationConfig formatsInput (Config config) augmentedInputs =
    List.map
        (\( Input input, spec ) ->
            ( { namespace =
                    case input.outputModuleName of
                        Just modName ->
                            modName

                        Nothing ->
                            spec
                                |> OpenApi.info
                                |> OpenApi.Info.title
                                |> Utils.sanitizeModuleName
                                |> Maybe.withDefault "Api"
                                |> List.singleton
              , generateTodos = config.generateTodos
              , effectTypes = input.effectTypes
              , server = input.server
              , formats = config.staticFormats ++ config.dynamicFormats formatsInput
              }
            , spec
            )
        )
        augmentedInputs
