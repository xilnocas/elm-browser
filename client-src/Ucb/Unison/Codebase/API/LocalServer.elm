module Ucb.Unison.Codebase.API.LocalServer exposing (makeLocalServerUnisonCodebaseAPI)

import Array exposing (Array)
import Bytes exposing (Bytes)
import Json.Decode
import Task exposing (Task)
import Ucb.Unison.Codebase.API exposing (..)
import Ucb.Util.Http as Http
import Ucb.Util.Task as Task
import Unison.Codebase.Causal exposing (..)
import Unison.Codebase.Serialization.V1 as V1
import Unison.Declaration exposing (..)
import Unison.Hash exposing (Hash32)
import Unison.Reference exposing (..)
import Unison.Symbol exposing (Symbol)
import Unison.Term exposing (Term)
import Unison.Type exposing (..)


makeLocalServerUnisonCodebaseAPI : UnisonCodebaseAPI
makeLocalServerUnisonCodebaseAPI =
    { getHeadHash = getHeadHash
    , getRawCausal = getRawCausal
    , getTerm = getTerm
    , getTermType = getTermType
    , getType = getType
    }


getHeadHash : Task (Http.Error String) (Http.Response Hash32)
getHeadHash =
    Http.getJson
        { decoder = Json.Decode.string
        , headers = []
        , timeout = Nothing
        , url = "head"
        }


getRawCausal :
    Hash32
    -> Task (Http.Error Bytes) ( Hash32, Http.Response RawCausal )
getRawCausal hash =
    Http.getBytes
        { decoder = V1.rawCausalDecoder
        , headers = []
        , timeout = Nothing
        , url = "branch/" ++ hash
        }
        |> Task.map (\response -> ( hash, response ))


getTerm :
    Id
    -> Task (Http.Error Bytes) ( Id, Http.Response (Term Symbol) )
getTerm id =
    Http.getBytes
        { decoder = V1.termDecoder
        , headers = []
        , timeout = Nothing
        , url = "term/" ++ idToString id ++ "/term"
        }
        |> Task.map (\response -> ( id, response ))


getTermType :
    Id
    -> Task (Http.Error Bytes) ( Id, Http.Response (Type Symbol) )
getTermType id =
    Http.getBytes
        { decoder = V1.typeDecoder
        , headers = []
        , timeout = Nothing
        , url = "term/" ++ idToString id ++ "/type"
        }
        |> Task.map (\response -> ( id, response ))


getType :
    Id
    -> Task (Http.Error Bytes) ( Id, Http.Response (Declaration Symbol) )
getType id =
    Http.getBytes
        { decoder = V1.declarationDecoder
        , headers = []
        , timeout = Nothing
        , url = "declaration/" ++ idToString id
        }
        |> Task.map (\response -> ( id, response ))
