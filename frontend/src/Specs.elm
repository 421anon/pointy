module Specs exposing (..)

import Accessors exposing (has, snd)
import Actions
import Api.ApiData as ApiData exposing (ApiData(..))
import Api.Decode as Decode
import Api.Encode as Encode
import Dict
import Extra.Accessors exposing (where_)
import Model.Core exposing (ProjectRecord, StepRecord, TableTag(..), initialTable)
import Model.Lenses as Lenses exposing (currentTableOf)
import Model.Shadow as Shadow exposing (StepConfig, StepType, WithSrcFiles(..))
import Model.TableSpec exposing (TableSpec(..))


steps : String -> StepType -> TableSpec StepRecord
steps name stepType =
    TableSpec
        { tag = TagSteps name stepType
        , name = name
        , lens = currentTableOf name
        , encodeRecord = Encode.stepValue stepType
        , decodeRecord = Decode.stepValueOnly stepType
        , status = \r -> ApiData.unwrap (ApiData.loading Nothing) .status r.runState
        , directoryView = \r -> ApiData.toMaybe r.runState |> Maybe.map .directoryView
        , srcFilesView =
            if has (Shadow.derivation << snd << where_ ((==) WithSrcFiles)) stepType then
                Just << .srcFiles

            else
                always Nothing
        , defaultRecord =
            { id = Nothing
            , clientId = Nothing
            , type_ = name
            , hidden = False
            , sortKey = Nothing
            , name = name
            , args = Dict.empty
            , runState = ApiData.loading Nothing
            , isUpdating = False
            , srcFiles =
                { children = NotAsked
                , expanded = False
                }
            }
        , displayName = name
        , apiPath = "/step"
        , upsertRecord = Actions.upsertStep
        }


projects : StepConfig -> TableSpec ProjectRecord
projects stepConfig =
    TableSpec
        { tag = TagProjects
        , name = "projects"
        , lens = Lenses.projects
        , encodeRecord = Encode.projectRecord
        , decodeRecord = Decode.projectRecord stepConfig
        , status = always NotAsked
        , directoryView = always Nothing
        , srcFilesView = always Nothing
        , defaultRecord =
            { id = Nothing
            , clientId = Nothing
            , hidden = False
            , sortKey = Nothing
            , name = ""
            , tables = Dict.map (always <| always initialTable) stepConfig
            , isUpdating = False
            }
        , displayName = "Projects"
        , apiPath = "/projects"
        , upsertRecord = Actions.upsertProject
        }
