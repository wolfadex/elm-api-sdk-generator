module Example exposing (..)

-- import BimcloudApi20232AlphaRelease

import AdditionalProperties.Json
import AdditionalProperties.Types
import AirlineCodeLookupApi.Api
import AirlineCodeLookupApi.Types
import BasicRouter.Json
import BasicRouter.Types
import Browser
import DbFahrplanApi.Api
import DbFahrplanApi.Types
import Dict
import GithubV3RestApi.Api
import GithubV3RestApi.Types
import Json.Decode
import MarioPartyStats.Api
import MarioPartyStats.Types
import OpenApi.Common
import RealworldConduitApi.Api
import RealworldConduitApi.Types
import RecursiveAllofRefs.Json
import RecursiveAllofRefs.Types
import Trustmark.TradeCheck.Api
import Trustmark.TradeCheck.Servers
import Trustmark.TradeCheck.Types


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Model =
    {}


init : () -> ( Model, Cmd Msg )
init () =
    ( {}
    , Cmd.batch
        [ RealworldConduitApi.Api.getArticle
            { toMsg = ConduitResponse
            , params = { slug = "" }
            }
        , AirlineCodeLookupApi.Api.getairlines
            { toMsg = AmadeusResponse
            , params = { airlineCodes = Nothing }
            }

        -- , BimcloudApi20232AlphaRelease.blobStoreService10BeginBatchUpload
        --     { toMsg = BimResponse
        --     , params = { sessionMinusid = Nothing, description = Nothing }
        --     }
        , GithubV3RestApi.Api.metaRoot { toMsg = GithubResponse }
        , DbFahrplanApi.Api.locationName
            { toMsg = DbFahrplanResponse
            , authorization = { authkey = "?" }
            , params =
                { format = "json"
                , lang = Nothing
                , input = "München"
                }
            }
        , MarioPartyStats.Api.getBoards
            { toMsg = MarioPartyStatsResponse
            , params =
                { id = Nothing
                , created_at = Nothing
                , updated_at = Nothing
                , name = Nothing
                , select = Nothing
                , order = Nothing
                , range = Nothing
                , range_Unit = Nothing
                , offset = Nothing
                , limit = Nothing
                , prefer = Nothing
                }
            }
        , Trustmark.TradeCheck.Api.tradeCheck
            { server = Trustmark.TradeCheck.Servers.sandbox
            , authorization = { x_api_key = "?" }
            , body = { publicId = 0, schemeId = Nothing }
            , toMsg = TrustmarkResponse
            }
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type Msg
    = ConduitResponse (Result (OpenApi.Common.Error RealworldConduitApi.Types.GenericError String) RealworldConduitApi.Types.SingleArticleResponse)
    | AmadeusResponse (Result (OpenApi.Common.Error AirlineCodeLookupApi.Types.Getairlines_Error String) AirlineCodeLookupApi.Types.Airlines)
      -- | BimResponse (Result (OpenApi.Common.Error BimcloudApi20232AlphaRelease.BlobStoreService10BeginBatchUpload_Error Bytes.Bytes) Bytes.Bytes)
    | GithubResponse (Result (OpenApi.Common.Error () String) GithubV3RestApi.Types.Root)
    | DbFahrplanResponse (Result (OpenApi.Common.Error Never String) DbFahrplanApi.Types.LocationResponse)
    | MarioPartyStatsResponse (Result (OpenApi.Common.Error Never String) (List MarioPartyStats.Types.Boards))
    | TrustmarkResponse (Result (OpenApi.Common.Error Never String) Trustmark.TradeCheck.Types.TradeCheckResponse)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ConduitResponse response ->
            ( model, Cmd.none )

        AmadeusResponse response ->
            ( model, Cmd.none )

        -- BimResponse response ->
        --     ( model, Cmd.none )
        GithubResponse response ->
            ( model, Cmd.none )

        DbFahrplanResponse _ ->
            ( model, Cmd.none )

        MarioPartyStatsResponse _ ->
            ( model, Cmd.none )

        TrustmarkResponse _ ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view _ =
    { title = "Example SDK Usage"
    , body = []
    }



-- Assert the structure of generated types by declaring instances of them.


{-| AdditionalProperties
-}
additionalPropertiesTaxonomy : AdditionalProperties.Types.Taxonomy
additionalPropertiesTaxonomy =
    { tags =
        { additionalProperties =
            Dict.fromList
                [ ( "Mystery", { name = "Mystery", isPopular = Just True } )
                ]
        , category = Just "Genres"
        }
    }


{-| BasicRouter
-}
basicRouterValuePropositionSummary : BasicRouter.Types.ValuePropositionSummary
basicRouterValuePropositionSummary =
    { one_sentence_summary = ""
    , type_ = ""
    }


{-| RecursiveAllofRefs
-}
recursiveAllofRefsChild : RecursiveAllofRefs.Types.Child
recursiveAllofRefsChild =
    { ancestor = ""
    , child = ""
    }


recursiveAllofRefsGrandChild : RecursiveAllofRefs.Types.GrandChild
recursiveAllofRefsGrandChild =
    { ancestor = ""
    , child = ""
    , grandChild = ""
    }
