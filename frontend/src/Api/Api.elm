module Api.Api exposing
    ( AutocompleteRequest
    , SeekAnchor(..)
    , assignRecordToProject
    , batchAssignRecordsToProject
    , createProject
    , createSrcFile
    , createStep
    , deleteProject
    , deleteSrcFile
    , fetchAutocomplete
    , fetchCommitHash
    , fetchDirectoryContents
    , fetchExtras
    , fetchFileContents
    , fetchFileSeek
    , fetchNotices
    , fetchPresets
    , fetchProjects
    , fetchSrcDirectoryContents
    , fetchSrcFileContents
    , fetchSrcFileSeek
    , fetchStepConfig
    , fetchStepLog
    , fetchUserRepoInfo
    , runStep
    , saveProject
    , saveSrcFile
    , saveRecord
    , srcFileDownloadUrl
    , stepFileBundleUrl
    , stepFileDownloadUrl
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
import Model.Core exposing (BaseRecord, DirectoryItem, FileChunk, Notice, ProjectRecord, StepRecord)
import Model.Shadow exposing (Presets, StepConfig, StepType)
import Model.TableSpec as TableSpec exposing (TableSpec)
import Url.Builder as UrlBuilder


type alias AutocompleteRequest =
    { template : String
    , autocomplete : String
    , context : Dict String String
    , query : String
    , limit : Int
    }


type SeekAnchor
    = AtLine Int
    | AtOffset Int


seekQueryParams : SeekAnchor -> Int -> List UrlBuilder.QueryParameter
seekQueryParams anchor bytes_ =
    let
        anchorParam =
            case anchor of
                AtLine n ->
                    UrlBuilder.int "line" n

                AtOffset n ->
                    UrlBuilder.int "offset" n
    in
    [ anchorParam, UrlBuilder.int "bytes" bytes_ ]


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


stepFileQuery : Int -> Maybe String -> List UrlBuilder.QueryParameter
stepFileQuery stepId commit =
    UrlBuilder.int "id" stepId
        :: (case commit of
                Just c ->
                    [ UrlBuilder.string "commit" c ]

                Nothing ->
                    []
           )


stepFileDownloadUrl : Int -> Maybe String -> List String -> String
stepFileDownloadUrl stepId commit filePath =
    UrlBuilder.absolute [ "backend", "step-files", "download" ]
        (stepFileQuery stepId commit ++ [ UrlBuilder.string "path" (String.join "/" filePath) ])


stepFileBundleUrl : Int -> String -> List String -> String
stepFileBundleUrl stepId commit filePath =
    UrlBuilder.absolute ([ "backend", "step-files", "bundle", String.fromInt stepId, commit ] ++ filePath) []


srcFileDownloadUrl : Int -> List String -> String
srcFileDownloadUrl id filePath =
    "/backend/src-files/download?id=" ++ String.fromInt id ++ "&path=" ++ String.join "/" filePath


stringResponse : (String -> a) -> Http.Response String -> Result Http.Error a
stringResponse onSuccess response =
    case response of
        Http.BadUrl_ badUrl ->
            Err (Http.BadUrl badUrl)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.BadStatus_ _ body ->
            Err (Http.BadBody body)

        Http.GoodStatus_ _ body ->
            Ok (onSuccess body)


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


stepAction : String -> Int -> Maybe String -> Flow s (Result Http.Error ())
stepAction action id commit =
    Flow.lift <|
        Http.post
            { url = appendCommitQuery ("/backend/" ++ action ++ "?id=" ++ String.fromInt id) commit
            , body = Http.emptyBody
            , expect = Http.expectStringResponse identity (stringResponse (always ()))
            }


createProject : Presets -> StepConfig -> ProjectRecord -> Flow s (Result Http.Error ProjectRecord)
createProject presets stepConfig record =
    Flow.lift <|
        Http.post
            { url = "/backend/projects"
            , body = Http.jsonBody <| Encode.projectRecord record
            , expect = Http.expectJson identity (Decode.projectRecord presets stepConfig)
            }


createStep : Maybe Int -> Maybe Int -> StepType -> StepRecord -> Flow s (Result Http.Error StepRecord)
createStep mProjectId mSourceId stepType record =
    let
        url =
            UrlBuilder.absolute [ "backend", "step" ]
                (List.filterMap identity
                    [ Maybe.map (UrlBuilder.int "project_id") mProjectId
                    , Maybe.map (UrlBuilder.int "source_id") mSourceId
                    ]
                )
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


fetchProjects : Maybe String -> Presets -> StepConfig -> Flow s (Result Http.Error (Dict String ProjectRecord))
fetchProjects commit presets stepConfig =
    Flow.lift <|
        Http.get
            { url = appendCommitQuery "/backend/projects" commit
            , expect = Http.expectJson identity <| Json.Decode.dict <| Decode.projectRecord presets stepConfig
            }


fetchStepConfig : Maybe String -> Flow s (Result Http.Error StepConfig)
fetchStepConfig commit =
    Flow.lift <|
        Http.get
            { url = appendCommitQuery "/backend/step-config" commit
            , expect = Http.expectJson identity Decode.stepConfig
            }


fetchPresets : Maybe String -> Flow s (Result Http.Error Presets)
fetchPresets commit =
    Flow.lift <|
        Http.get
            { url = appendCommitQuery "/backend/presets" commit
            , expect = Http.expectJson identity Decode.presets
            }


fetchAutocomplete : Maybe String -> AutocompleteRequest -> Flow s (Result Http.Error (List String))
fetchAutocomplete commit autocompleteRequest =
    Flow.lift <|
        Http.post
            { url = appendCommitQuery "/backend/autocomplete" commit
            , body =
                Http.jsonBody <|
                    Json.Encode.object
                        [ ( "template", Json.Encode.string autocompleteRequest.template )
                        , ( "autocomplete", Json.Encode.string autocompleteRequest.autocomplete )
                        , ( "context", Json.Encode.dict identity Json.Encode.string autocompleteRequest.context )
                        , ( "query", Json.Encode.string autocompleteRequest.query )
                        , ( "limit", Json.Encode.int autocompleteRequest.limit )
                        ]
            , expect = Http.expectJson identity (Json.Decode.list Json.Decode.string)
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
    request "DELETE" ("/backend/projects?id=" ++ String.fromInt projectId) Http.emptyBody


assignRecordToProject : Int -> Int -> Flow s (Result Http.Error ())
assignRecordToProject projectId recordId =
    request "POST"
        ("/backend/project-entities?project_id="
            ++ String.fromInt projectId
            ++ "&entity_id="
            ++ String.fromInt recordId
        )
        Http.emptyBody


batchAssignRecordsToProject : Int -> List Int -> Flow s (Result Http.Error ())
batchAssignRecordsToProject projectId recordIds =
    let
        url =
            "/backend/project-entities/batch?project_id="
                ++ String.fromInt projectId
    in
    request "POST" url (Http.jsonBody (Json.Encode.list Json.Encode.int recordIds))


unassignRecordFromProject : Int -> Int -> Flow s (Result Http.Error ())
unassignRecordFromProject projectId recordId =
    request "DELETE"
        ("/backend/project-entities?project_id="
            ++ String.fromInt projectId
            ++ "&entity_id="
            ++ String.fromInt recordId
        )
        Http.emptyBody


runStep : Int -> Maybe String -> Flow s (Result Http.Error ())
runStep id commit =
    stepAction "run-step" id commit


stopStep : Int -> Maybe String -> Flow s (Result Http.Error ())
stopStep id commit =
    stepAction "stop-step" id commit


fetchStepLog : Int -> Maybe String -> Flow s (Result Http.Error String)
fetchStepLog id commit =
    Flow.lift <|
        Http.get
            { url = appendCommitQuery ("/backend/step-log?id=" ++ String.fromInt id) commit
            , expect = Http.expectStringResponse identity (stringResponse identity)
            }


fetchNotices : Int -> Maybe String -> Flow s (Result Http.Error (List Notice))
fetchNotices id commit =
    Flow.lift <|
        Http.get
            { url = appendCommitQuery ("/backend/notices?id=" ++ String.fromInt id) commit
            , expect = Http.expectJson identity (Json.Decode.list Decode.notice)
            }


fetchDirectoryContents : Json.Decode.Decoder ( String, DirectoryItem ) -> Int -> Maybe String -> List String -> Flow s (Result Http.Error (Dict String DirectoryItem))
fetchDirectoryContents itemDecoder stepId commit folderPath =
    Flow.lift <|
        Http.get
            { url = UrlBuilder.absolute [ "backend", "step-files" ] (stepFileQuery stepId commit ++ [ UrlBuilder.string "path" (String.join "/" folderPath) ])
            , expect = Http.expectJson identity (Json.Decode.map Dict.fromList <| Json.Decode.list itemDecoder)
            }


fetchExtras : Int -> Maybe String -> List String -> Flow s (Result Http.Error (Dict String Json.Decode.Value))
fetchExtras stepId commit folderPath =
    Flow.lift <|
        Http.get
            { url =
                UrlBuilder.absolute [ "backend", "step-files", "extras" ]
                    (stepFileQuery stepId commit
                        ++ [ UrlBuilder.string "path" (String.join "/" folderPath) ]
                    )
            , expect = Http.expectJson identity (Json.Decode.dict Json.Decode.value)
            }


fetchFileContents : Int -> Maybe String -> List String -> Flow s (Result Http.Error String)
fetchFileContents stepId commit filePath =
    Flow.lift <|
        Http.get
            { url = stepFileDownloadUrl stepId commit filePath
            , expect = Http.expectString identity
            }


fetchFileSeek : Int -> Maybe String -> List String -> SeekAnchor -> Int -> Flow s (Result Http.Error FileChunk)
fetchFileSeek stepId commit filePath anchor bytes_ =
    Flow.lift <|
        Http.get
            { url =
                UrlBuilder.absolute [ "backend", "step-files", "seek" ]
                    (stepFileQuery stepId commit
                        ++ [ UrlBuilder.string "path" (String.join "/" filePath) ]
                        ++ seekQueryParams anchor bytes_
                    )
            , expect = Http.expectJson identity Decode.fileChunk
            }


fetchSrcFileSeek : Int -> List String -> SeekAnchor -> Int -> Flow s (Result Http.Error FileChunk)
fetchSrcFileSeek recordId filePath anchor bytes_ =
    Flow.lift <|
        Http.get
            { url =
                UrlBuilder.absolute [ "backend", "src-files", "seek" ]
                    ([ UrlBuilder.int "id" recordId
                     , UrlBuilder.string "path" (String.join "/" filePath)
                     ]
                        ++ seekQueryParams anchor bytes_
                    )
            , expect = Http.expectJson identity Decode.fileChunk
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
            { url = srcFileDownloadUrl id filePath
            , expect = Http.expectString identity
            }


srcFileRequest : String -> Int -> List String -> Http.Body -> Flow s (Result Http.Error ())
srcFileRequest method id filePath body =
    let
        url =
            UrlBuilder.absolute
                [ "backend", "src-files" ]
                [ UrlBuilder.int "id" id
                , UrlBuilder.string "path" (String.join "/" filePath)
                ]
    in
    request method url body


saveSrcFile : Int -> List String -> String -> Flow s (Result Http.Error ())
saveSrcFile id filePath content =
    srcFileRequest "PUT" id filePath (Http.stringBody "text/plain; charset=utf-8" content)


createSrcFile : Int -> List String -> String -> Flow s (Result Http.Error ())
createSrcFile id filePath content =
    srcFileRequest "POST" id filePath (Http.stringBody "text/plain; charset=utf-8" content)


deleteSrcFile : Int -> List String -> Flow s (Result Http.Error ())
deleteSrcFile id filePath =
    srcFileRequest "DELETE" id filePath Http.emptyBody
