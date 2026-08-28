module Specs exposing (..)

import Accessors exposing (has, snd, try)
import Actions
import Api.ApiData as ApiData exposing (ApiData(..))
import Api.Decode as Decode
import Api.Encode as Encode
import Components.Select as Select
import Dict
import Extra.Accessors exposing (where_)
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


projects : Presets -> StepConfig -> TableSpec ProjectRecord
projects presets stepConfig =
    TableSpec
        { tag = TagProjects
        , name = "projects"
        , lens = Lenses.projects
        , encodeRecord = Encode.projectRecord
        , decodeRecord = Decode.projectRecord presets stepConfig
        , status = always NotAsked
        , validationErrors = .validationErrors
        , directoryView = always Nothing
        , srcFilesView = always Nothing
        , defaultRecord =
            { id = Nothing
            , clientId = Nothing
            , hidden = False
            , sortKey = Nothing
            , name = ""
            , tables = Dict.empty
            , templateSource = Model.defaultTemplateSource presets
            , orphanedSteps = []
            , validationErrors = []
            , hideOrphans = False
            , presetSelect = Select.initSelectState
            , templatesSelect = Select.initSelectState
            , isUpdating = False
            , lastModifiedAt = Nothing
            }
        , displayName = "Projects"
        , description = Nothing
        , apiPath = "/projects"
        , upsertRecord = Actions.upsertProject
        , cloneRecord = \_ _ -> Flow.none
        }


type StepRunControl
    = Runnable (Flow.Flow Model.Model ())
    | Stoppable (Flow.Flow Model.Model ())


stepRunControl : Model.Model -> Int -> Maybe StepRunControl
stepRunControl model stepId =
    try (Lenses.currentProject << ApiData.success << Lenses.projectStepRecords << where_ (.id >> (==) (Just stepId))) model
        |> Maybe.andThen
            (\step ->
                try (Lenses.stepConfig << ApiData.success) model
                    |> Maybe.andThen (Dict.get step.type_)
                    |> Maybe.andThen (runControl step)
            )


runControl : StepRecord -> StepConfigEntry -> Maybe StepRunControl
runControl step entry =
    if Shadow.hasRunControl entry.stepType then
        let
            spec =
                steps step.type_ entry
        in
        step.id
            |> Maybe.andThen
                (\stepId ->
                    if TableSpec.getRunning spec step then
                        Just (Stoppable (Actions.stopStep spec stepId))

                    else if TableSpec.getRunnable spec step then
                        Just (Runnable (Actions.runStep spec stepId))

                    else
                        Nothing
                )

    else
        Nothing
