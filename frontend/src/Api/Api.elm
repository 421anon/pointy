module Api.Api exposing
    ( assignRecordToProject
    , batchAssignRecordsToProject
    , createProject
    , createStep
    , deleteProject
    , fetchCommitHash
    , fetchDirectoryContents
    , fetchFileContents
    , fetchProjects
    , fetchSrcDirectoryContents
    , fetchSrcFileContents
    , fetchStepConfig
    , fetchUserRepoInfo
    , runStep
    , saveProject
    , saveRecord
    , stopStep
    , unassignRecordFromProject
    , uploadFiles
    )

import Api.Decode as Decode
import Api.Encode as Encode
import Dict exposing (Dict)
import File exposing (File)
import Flow exposing (Flow)
import Http
import Json.Decode
import Json.Encode
import Maybe.Extra as Maybe
import Model.Core exposing (BaseRecord, DirectoryItem, ProjectRecord, StepRecord)
import Model.Shadow exposing (StepConfig, StepType)
import Model.TableSpec as TableSpec exposing (TableSpec)


collectionPath : TableSpec a -> String
collectionPath tableSpec =
    "/backend" ++ TableSpec.getApiPath tableSpec


recordUrl : TableSpec a -> Int -> String
recordUrl tableSpec id =
    collectionPath tableSpec ++ "?id=" ++ String.fromInt id


appendCommitQuery : String -> Maybe String -> String
appendCommitQuery url commit =
    case commit of
        Just c ->
            if String.contains "?" url then
                url ++ "&commit=" ++ c

            else
                url ++ "?commit=" ++ c

        Nothing ->
            url


request : String -> String -> Http.Body -> Flow s (Result Http.Error ())
request method url body =
    Flow.lift <|
        Http.request
            { method = method
            , headers = []
            , url = url
            , body = body
            , expect = Http.expectWhatever identity
            , timeout = Nothing
            , tracker = Nothing
            }


createProject : StepConfig -> ProjectRecord -> Flow s (Result Http.Error ProjectRecord)
createProject stepConfig record =
    Flow.lift <|
        Http.post
            { url = "/backend/projects"
            , body = Http.jsonBody <| Encode.projectRecord record
            , expect = Http.expectJson identity (Decode.projectRecord stepConfig)
            }


createStep : Maybe Int -> StepType -> StepRecord -> Flow s (Result Http.Error StepRecord)
createStep mProjectId stepType record =
    let
        url =
            Maybe.unwrap "/backend/step"
                (\projectId -> "/backend/step?project_id=" ++ String.fromInt projectId)
                mProjectId
    in
    Flow.lift <|
        Http.post
            { url = url
            , body = Http.jsonBody (Encode.stepValue stepType record)
            , expect = Http.expectJson identity (Decode.stepValueOnly stepType)
            }


saveRecord : TableSpec (BaseRecord a) -> BaseRecord a -> Flow s (Result Http.Error ())
saveRecord tableSpec record =
    Maybe.unwrap (Flow.pure <| Ok ())
        (\id ->
            request "PATCH" (recordUrl tableSpec id) (Http.jsonBody (TableSpec.getEncodeRecord tableSpec record))
        )
        record.id


saveProject : Int -> ProjectRecord -> Flow s (Result Http.Error ())
saveProject projectId project =
    request "PATCH" ("/backend/projects?id=" ++ String.fromInt projectId) (Http.jsonBody (Encode.projectRecord project))


uploadFiles : Int -> List File -> Flow s (Result Http.Error ())
uploadFiles stepId files =
    Flow.lift <|
        Http.request
            { method = "POST"
            , headers = []
            , url = "/backend/upload?id=" ++ String.fromInt stepId
            , body = Http.multipartBody (List.map (Http.filePart "files") files)
            , expect = Http.expectWhatever identity
            , timeout = Nothing
            , tracker = Just ("upload-" ++ String.fromInt stepId)
            }


fetchProjects : Maybe String -> StepConfig -> Flow s (Result Http.Error (Dict String ProjectRecord))
fetchProjects commit stepConfig =
    Flow.lift <|
        Http.get
            { url = appendCommitQuery "/backend/projects" commit
            , expect = Http.expectJson identity <| Json.Decode.dict <| Decode.projectRecord stepConfig
            }


fetchStepConfig : Maybe String -> Flow s (Result Http.Error StepConfig)
fetchStepConfig commit =
    Flow.lift <|
        Http.get
            { url = appendCommitQuery "/backend/step-config" commit
            , expect = Http.expectJson identity Decode.stepConfig
            }


fetchCommitHash : Flow s (Result Http.Error String)
fetchCommitHash =
    Flow.lift <|
        Http.get
            { url = "/backend/commit-hash"
            , expect = Http.expectString identity
            }


deleteProject : Int -> Flow s (Result Http.Error ())
deleteProject projectId =
    Flow.lift <|
        Http.request
            { method = "DELETE"
            , headers = []
            , url = "/backend/projects?id=" ++ String.fromInt projectId
            , body = Http.emptyBody
            , expect = Http.expectWhatever identity
            , timeout = Nothing
            , tracker = Nothing
            }


assignRecordToProject : Int -> Int -> Flow s (Result Http.Error ())
assignRecordToProject projectId recordId =
    let
        url =
            "/backend/project-entities?project_id="
                ++ String.fromInt projectId
                ++ "&entity_id="
                ++ String.fromInt recordId
    in
    Flow.lift <|
        Http.request
            { method = "POST"
            , headers = []
            , url = url
            , body = Http.emptyBody
            , expect = Http.expectWhatever identity
            , timeout = Nothing
            , tracker = Nothing
            }


batchAssignRecordsToProject : Int -> List Int -> Flow s (Result Http.Error ())
batchAssignRecordsToProject projectId recordIds =
    let
        url =
            "/backend/project-entities/batch?project_id="
                ++ String.fromInt projectId
    in
    Flow.lift <|
        Http.request
            { method = "POST"
            , headers = []
            , url = url
            , body = Http.jsonBody (Json.Encode.list Json.Encode.int recordIds)
            , expect = Http.expectWhatever identity
            , timeout = Nothing
            , tracker = Nothing
            }


unassignRecordFromProject : Int -> Int -> Flow s (Result Http.Error ())
unassignRecordFromProject projectId recordId =
    let
        url =
            "/backend/project-entities?project_id="
                ++ String.fromInt projectId
                ++ "&entity_id="
                ++ String.fromInt recordId
    in
    Flow.lift <|
        Http.request
            { method = "DELETE"
            , headers = []
            , url = url
            , body = Http.emptyBody
            , expect = Http.expectWhatever identity
            , timeout = Nothing
            , tracker = Nothing
            }


runStep : Int -> Maybe String -> Flow s (Result Http.Error ())
runStep id commit =
    let
        requestUrl =
            appendCommitQuery ("/backend/run-step?id=" ++ String.fromInt id) commit
    in
    Flow.lift <|
        Http.post
            { url = requestUrl
            , body = Http.emptyBody
            , expect =
                Http.expectStringResponse identity <|
                    \response ->
                        case response of
                            Http.BadUrl_ badUrl ->
                                Err (Http.BadUrl badUrl)

                            Http.Timeout_ ->
                                Err Http.Timeout

                            Http.NetworkError_ ->
                                Err Http.NetworkError

                            Http.BadStatus_ _ body ->
                                Err (Http.BadBody body)

                            Http.GoodStatus_ _ _ ->
                                Ok ()
            }


stopStep : Int -> Maybe String -> Flow s (Result Http.Error ())
stopStep id commit =
    let
        requestUrl =
            appendCommitQuery ("/backend/stop-step?id=" ++ String.fromInt id) commit
    in
    Flow.lift <|
        Http.post
            { url = requestUrl
            , body = Http.emptyBody
            , expect =
                Http.expectStringResponse identity <|
                    \response ->
                        case response of
                            Http.BadUrl_ badUrl ->
                                Err (Http.BadUrl badUrl)

                            Http.Timeout_ ->
                                Err Http.Timeout

                            Http.NetworkError_ ->
                                Err Http.NetworkError

                            Http.BadStatus_ _ body ->
                                Err (Http.BadBody body)

                            Http.GoodStatus_ _ _ ->
                                Ok ()
            }


fetchDirectoryContents : Json.Decode.Decoder ( String, DirectoryItem ) -> String -> List String -> Flow s (Result Http.Error (Dict String DirectoryItem))
fetchDirectoryContents itemDecoder outPath folderPath =
    Flow.lift <|
        Http.get
            { url = "/backend/store?outPath=" ++ outPath ++ "&path=" ++ String.join "/" folderPath
            , expect = Http.expectJson identity (Json.Decode.map Dict.fromList <| Json.Decode.list itemDecoder)
            }


fetchFileContents : String -> List String -> Flow s (Result Http.Error String)
fetchFileContents outPath filePath =
    Flow.lift <|
        Http.get
            { url = "/backend/store/download?outPath=" ++ outPath ++ "&path=" ++ String.join "/" filePath
            , expect = Http.expectString identity
            }


fetchUserRepoInfo : Flow s (Result Http.Error Model.Core.UserRepoInfo)
fetchUserRepoInfo =
    Flow.lift <|
        Http.get
            { url = "/backend/user-repo-info"
            , expect = Http.expectJson identity Decode.userRepoInfo
            }


fetchSrcDirectoryContents : Json.Decode.Decoder ( String, DirectoryItem ) -> Int -> List String -> Flow s (Result Http.Error (Dict String DirectoryItem))
fetchSrcDirectoryContents itemDecoder id folderPath =
    Flow.lift <|
        Http.get
            { url =
                "/backend/src-files?id="
                    ++ String.fromInt id
                    ++ (if List.isEmpty folderPath then
                            ""

                        else
                            "&path=" ++ String.join "/" folderPath
                       )
            , expect = Http.expectJson identity (Json.Decode.map Dict.fromList <| Json.Decode.list itemDecoder)
            }


fetchSrcFileContents : Int -> List String -> Flow s (Result Http.Error String)
fetchSrcFileContents id filePath =
    Flow.lift <|
        Http.get
            { url = "/backend/src-files/download?id=" ++ String.fromInt id ++ "&path=" ++ String.join "/" filePath
            , expect = Http.expectString identity
            }
