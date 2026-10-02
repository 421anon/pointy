module Specs exposing (..)

import Accessors exposing (has, snd)
import Actions
import Api.ApiData as ApiData exposing (ApiData(..))
import Api.Decode as Decode
import Api.Encode as Encode
import Dict
import Extra.Accessors exposing (by, where_)
import Flow
import Model.Core as Model exposing (ProjectRecord, StepRecord, TableTag(..))
import Model.Lenses as Lenses exposing (currentTableOf)
import Model.Shadow as Shadow exposing (Presets, StepConfig, StepConfigEntry, WithSrcFiles(..))
import Model.TableSpec as TableSpec exposing (TableSpec(..))


steps : String -> StepConfigEntry -> TableSpec StepRecord
steps name entry =
    let
        stepType =
            entry.stepType
    in
    TableSpec
        { tag = TagSteps name stepType
        , name = name
        , lens = currentTableOf name
        , encodeRecord = Encode.stepValue stepType
        , decodeRecord = Decode.stepValueOnly stepType
        , status = \r -> ApiData.unwrap (ApiData.loading Nothing) .status r.runState
        , validationErrors = always []
        , isLocked = .review >> (/=) Nothing
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
            , note = ""
            , args = Dict.empty
            , runState = ApiData.loading Nothing
            , review = Nothing
            , isUpdating = False
            , lastModifiedAt = Nothing
            , srcFiles =
                { children = NotAsked
                , expanded = False
                , extras = NotAsked
                , size = Nothing
                , mimeType = Nothing
                }
            , srcFileDraft = Nothing
            , srcFileWriting = False
            }
        , displayName = Maybe.withDefault name entry.displayName
        , description = entry.description
        , apiPath = "/step"
        , upsertRecord = Actions.upsertStep
        , cloneRecord = Actions.cloneStep
        }


stepsInProject : Int -> String -> StepConfigEntry -> TableSpec StepRecord
stepsInProject projectId name entry =
    case steps name entry of
        TableSpec spec ->
            TableSpec
                { spec
                    | lens =
                        Lenses.projects
                            << Lenses.records
                            << ApiData.success
                            << by .id (Just projectId)
                            << Lenses.tableInProject name
                }


allProjects : Presets -> StepConfig -> TableSpec ProjectRecord
allProjects presets stepConfig =
    let
        blankProject =
            Model.blankProject
    in
    TableSpec
        { tag = TagAllProjects
        , name = "all-projects"
        , lens = Lenses.projects
        , encodeRecord = Encode.projectRecord
        , decodeRecord = Decode.projectRecord presets stepConfig
        , status = always NotAsked
        , validationErrors = .validationErrors
        , isLocked = always False
        , directoryView = always Nothing
        , srcFilesView = always Nothing
        , defaultRecord = { blankProject | templateSource = Model.defaultTemplateSource presets }
        , displayName = "Project"
        , description = Nothing
        , apiPath = "/projects"
        , upsertRecord = Actions.upsertProject
        , cloneRecord = \_ _ -> Flow.none
        }


projects : Presets -> StepConfig -> TableSpec ProjectRecord
projects presets stepConfig =
    case allProjects presets stepConfig of
        TableSpec spec ->
            TableSpec
                { spec
                    | tag = TagProjects
                    , name = "projects"
                    , lens = Lenses.currentSubProjects
                    , displayName = "Projects"
                }
