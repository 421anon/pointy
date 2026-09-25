module Ingest exposing (..)

import Accessors exposing (try)
import Actions
import Api.Api as Api
import Api.ApiData as ApiData exposing (ApiData(..), success)
import Api.Decode as ApiDecode
import Basics.Extra exposing (flip)
import Channels
import Dict exposing (Dict)
import Extra.Http as Http
import File.Select as Select
import Flow exposing (Flow)
import Http
import Json.Decode as Decode
import Maybe.Extra as Maybe
import Model.Core as Model exposing (IngestJob, IngestState(..), Model)
import Model.Lenses exposing (currentProject, ingestJobs, pendingIngestSteps, scratchError, scratchListing, scratchPickerStepId, scratchRoot, stepConfig, tables, uploadProgress, void)
import Model.TableSpec exposing (StepSpec)
import Set exposing (Set)
import Specs


startIngestStream : Flow Model Decode.Value
startIngestStream =
    Flow.subscribe onIngestJobsIn Channels.ingestJobs


onIngestJobsIn : Decode.Value -> Flow Model ()
onIngestJobsIn value =
    case Decode.decodeValue ApiDecode.ingestJobs value of
        Ok jobs ->
            Flow.get
                |> Flow.andThen
                    (\model ->
                        let
                            previous =
                                Model.getIngestJobs model

                            pending =
                                Model.getPendingIngestSteps model

                            next =
                                List.map (\job -> ( job.stepId, job )) jobs
                                    |> Dict.fromList

                            completions =
                                List.filter (shouldReact pending previous) (List.filter (.state >> (==) IngestSucceeded) jobs)

                            failures =
                                List.filter (shouldReact pending previous) (List.filter (.state >> (==) IngestFailed) jobs)

                            observed =
                                List.map .stepId jobs |> Set.fromList

                            completed =
                                List.map .stepId (completions ++ failures) |> Set.fromList
                        in
                        Flow.setAll ingestJobs next
                            |> Flow.seq (Flow.setAll pendingIngestSteps (Set.diff (Set.intersect pending observed) completed))
                            |> Flow.seq (Flow.when (not (List.isEmpty completions)) Actions.refreshReviews)
                            |> Flow.seq (Flow.batchM (List.map (.stepId >> runStepForStepId) completions))
                            |> Flow.seq (Flow.batchM (List.map reportIngestFailure failures))
                    )

        Err err ->
            Actions.addToast False ("Ingest stream decode error: " ++ Decode.errorToString err)


shouldReact : Set Int -> Dict Int IngestJob -> IngestJob -> Bool
shouldReact pending previous job =
    Set.member job.stepId pending || observedRunning previous job


observedRunning : Dict Int IngestJob -> IngestJob -> Bool
observedRunning previous job =
    Dict.get job.stepId previous
        |> Maybe.unwrap False (\prev -> prev.state == IngestRunning && prev.id == job.id)


reportIngestFailure : IngestJob -> Flow Model ()
reportIngestFailure job =
    Actions.addToast False (Maybe.withDefault "Ingest failed." job.error)


adoptPendingStep : Int -> Flow Model ()
adoptPendingStep stepId =
    Flow.get
        |> Flow.andThen
            (\model ->
                (case Dict.get stepId (Model.getIngestJobs model) of
                    Just job ->
                        Flow.when (job.state /= IngestRunning) (Flow.over ingestJobs (Dict.remove stepId))

                    Nothing ->
                        Flow.pure ()
                )
                    |> Flow.seq (Flow.over pendingIngestSteps (Set.insert stepId))
            )


runStepForStepId : Int -> Flow Model ()
runStepForStepId stepId =
    Flow.get
        |> Flow.andThen
            (\model ->
                stepSpecForId model stepId
                    |> Maybe.unwrap (Flow.pure ()) (flip Actions.runStep stepId)
            )


stepSpecForId : Model -> Int -> Maybe StepSpec
stepSpecForId model stepId =
    let
        mStepName =
            try (currentProject << success << tables) model
                |> Maybe.unwrap Dict.empty identity
                |> Dict.toList
                |> List.filterMap
                    (\( name, table ) ->
                        table.records
                            |> ApiData.toMaybe
                            |> Maybe.andThen (List.filter (\record -> record.id == Just stepId) >> List.head)
                            |> Maybe.map (\_ -> name)
                    )
                |> List.head
    in
    mStepName
        |> Maybe.andThen
            (\name ->
                try (stepConfig << success) model
                    |> Maybe.andThen (Dict.get name)
                    |> Maybe.map (Specs.steps name)
            )


uploadFiles : List String -> Int -> Flow Model ()
uploadFiles types stepId =
    Flow.lift (Select.files types (\file files -> Api.uploadFiles stepId (file :: files)))
        |> Flow.andThen
            (\cmd ->
                Flow.over uploadProgress (Dict.insert stepId { sent = 0, size = 0 })
                    |> Flow.seq
                        (Actions.callApi void cmd
                            |> Flow.andThen
                                (\result ->
                                    Flow.over uploadProgress (Dict.remove stepId)
                                        |> Flow.seq
                                            (case result of
                                                Ok _ ->
                                                    adoptPendingStep stepId

                                                Err _ ->
                                                    Flow.pure ()
                                            )
                                )
                        )
            )


onUploadProgress : Int -> Http.Progress -> Flow Model ()
onUploadProgress stepId progress =
    case progress of
        Http.Sending p ->
            Flow.over uploadProgress (Dict.insert stepId { sent = p.sent, size = p.size })

        Http.Receiving _ ->
            Flow.pure ()


cancelUpload : Int -> Flow Model ()
cancelUpload stepId =
    Flow.batchM
        [ Flow.lift (Http.cancel ("upload-" ++ String.fromInt stepId))
        , Flow.over uploadProgress (Dict.remove stepId)
        ]


loadScratch : Flow Model ()
loadScratch =
    Api.fetchScratchRoot
        |> Flow.andThen (\result -> Flow.setAll scratchRoot (ApiData.fromResult result))


openScratchPicker : Int -> Flow Model ()
openScratchPicker stepId =
    Flow.setAll scratchPickerStepId (Just stepId)
        |> Flow.seq (Flow.setAll scratchError Nothing)
        |> Flow.seq (loadScratchListing "")
        |> Flow.seq (Actions.openDialog "scratch-picker-dialog")


scratchPickerClosed : Flow Model ()
scratchPickerClosed =
    Flow.setAll scratchPickerStepId Nothing
        |> Flow.seq (Flow.setAll scratchListing NotAsked)
        |> Flow.seq (Flow.setAll scratchError Nothing)


loadScratchListing : String -> Flow Model ()
loadScratchListing path =
    Flow.setAll scratchListing (Loading Nothing)
        |> Flow.seq
            (Api.fetchScratchListing path
                |> Flow.andThen
                    (\result ->
                        case result of
                            Ok listing ->
                                Flow.setAll scratchListing (Success listing)
                                    |> Flow.seq (Flow.setAll scratchError Nothing)

                            Err error ->
                                Flow.setAll scratchListing (Error error)
                                    |> Flow.seq (Flow.setAll scratchError (Just (Http.errorMessage error)))
                    )
            )


wrapScratchDirectory : Int -> String -> Flow Model ()
wrapScratchDirectory stepId path =
    Api.wrapScratch stepId path
        |> Flow.andThen
            (\result ->
                case result of
                    Ok _ ->
                        Actions.closeDialog "scratch-picker-dialog"
                            |> Flow.seq (adoptPendingStep stepId)

                    Err error ->
                        Flow.setAll scratchError (Just (Http.errorMessage error))
            )
