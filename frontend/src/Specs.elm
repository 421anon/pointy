module Specs exposing (..)

import Accessors exposing (has, snd, try)
import Actions
import Api.ApiData as ApiData exposing (ApiData(..))
import Api.Encode as Encode
import Extra.Accessors exposing (where_)
import Flow
import Model.Core as Model exposing (ChildKind(..), ProjectRecord, StepRecord, blankProject, blankStep)
import Model.Lenses as Lenses
import Model.Shadow as Shadow exposing (Presets, StepConfigEntry, WithSrcFiles(..))
import Model.TableSpec exposing (TableSpec(..))


steps : String -> StepConfigEntry -> TableSpec StepRecord
steps name entry =
    let
        stepType =
            entry.stepType
    in
    TableSpec
        { name = name
        , childKind = StepChild
        , lens = Lenses.stepFormsAt name
        , encodeRecord = Encode.stepValue stepType
        , status = \r -> ApiData.unwrap (ApiData.loading Nothing) .status r.runState
        , validationErrors = always []
        , isLocked = .review >> (/=) Nothing
        , directoryView = \r -> ApiData.toMaybe r.runState |> Maybe.map .directoryView
        , srcFilesView =
            if has (Shadow.derivation << snd << where_ ((==) WithSrcFiles)) stepType then
                Just << .srcFiles

            else
                always Nothing
        , defaultRecord = blankStep name
        , findRecord = \stepId model -> try (Lenses.stepRecordById stepId) model
        , displayName = Maybe.withDefault name entry.displayName
        , description = entry.description
        , apiPath = "/step"
        , upsertRecord = Actions.upsertStep
        , cloneRecord = Actions.cloneStep
        }


allProjects : Presets -> TableSpec ProjectRecord
allProjects presets =
    TableSpec
        { name = "all-projects"
        , childKind = ProjectChild
        , lens = Lenses.projectForms
        , encodeRecord = Encode.projectRecord
        , status = always NotAsked
        , validationErrors = .validationErrors
        , isLocked = always False
        , directoryView = always Nothing
        , srcFilesView = always Nothing
        , defaultRecord = { blankProject | templateSource = Model.defaultTemplateSource presets }
        , findRecord = \projectId model -> try (Lenses.projectRecordById projectId) model
        , displayName = "Project"
        , description = Nothing
        , apiPath = "/projects"
        , upsertRecord = Actions.upsertProject
        , cloneRecord = \_ _ -> Flow.none
        }
