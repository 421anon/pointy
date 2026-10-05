module Model.TableSpec exposing
    ( StepSpec
    , TableSpec(..)
    , formId
    , getApiPath
    , getChildKind
    , getCloneRecord
    , getDefaultRecord
    , getDescription
    , getDirectoryView
    , getDisplayName
    , getEncodeRecord
    , getFindRecord
    , getIsLocked
    , getLens
    , getName
    , getShareable
    , getSrcFilesView
    , getStatus
    , getUpsertRecord
    , getValidationErrors
    )

import Accessors exposing (Traversal, has)
import Api.ApiData as ApiData exposing (ApiData)
import Extra.Accessors exposing (A_Traversal, orElseT, remkT, where_)
import Flow exposing (Flow)
import Json.Encode
import Model.Core exposing (ChildKind, DirectoryFolder, Model, Status(..), StepRecord, Table, hasBuiltOutput)


type TableSpec a
    = TableSpec
        { name : String
        , childKind : ChildKind
        , lens : A_Traversal Model (Table a)
        , status : a -> ApiData Status
        , validationErrors : a -> List String
        , isLocked : a -> Bool
        , directoryView : a -> Maybe DirectoryFolder
        , srcFilesView : a -> Maybe DirectoryFolder
        , encodeRecord : a -> Json.Encode.Value
        , defaultRecord : Model -> a
        , findRecord : Int -> Model -> Maybe a
        , apiPath : String
        , displayName : String
        , description : Maybe String
        , upsertRecord : TableSpec a -> Flow Model ()
        , cloneRecord : TableSpec a -> a -> Flow Model ()
        }


type alias StepSpec =
    TableSpec StepRecord


getName : TableSpec a -> String
getName (TableSpec spec) =
    spec.name


getChildKind : TableSpec a -> ChildKind
getChildKind (TableSpec spec) =
    spec.childKind


getDisplayName : TableSpec a -> String
getDisplayName (TableSpec spec) =
    spec.displayName


getDescription : TableSpec a -> Maybe String
getDescription (TableSpec spec) =
    spec.description


getFindRecord : TableSpec a -> Int -> Model -> Maybe a
getFindRecord (TableSpec spec) =
    spec.findRecord


formId : TableSpec a -> String
formId spec =
    getName spec ++ "-form"


getLens : TableSpec a -> Traversal Model (Table a) x y
getLens (TableSpec spec) =
    remkT spec.lens


getStatus : TableSpec a -> a -> ApiData Status
getStatus (TableSpec spec) =
    spec.status


getValidationErrors : TableSpec a -> a -> List String
getValidationErrors (TableSpec spec) =
    spec.validationErrors


getIsLocked : TableSpec a -> a -> Bool
getIsLocked (TableSpec spec) =
    spec.isLocked


getShareable : TableSpec a -> a -> Bool
getShareable (TableSpec spec) =
    spec.status
        >> has (orElseT ApiData.success ApiData.reloading << where_ isShareableStatus)


isShareableStatus : Status -> Bool
isShareableStatus status =
    hasBuiltOutput status || status == StatusRunning


getDirectoryView : TableSpec a -> a -> Maybe DirectoryFolder
getDirectoryView (TableSpec spec) =
    spec.directoryView


getSrcFilesView : TableSpec a -> a -> Maybe DirectoryFolder
getSrcFilesView (TableSpec spec) =
    spec.srcFilesView


getEncodeRecord : TableSpec a -> a -> Json.Encode.Value
getEncodeRecord (TableSpec spec) record =
    spec.encodeRecord record


getDefaultRecord : TableSpec a -> Model -> a
getDefaultRecord (TableSpec spec) =
    spec.defaultRecord


getApiPath : TableSpec a -> String
getApiPath (TableSpec spec) =
    spec.apiPath


getUpsertRecord : TableSpec a -> Flow Model ()
getUpsertRecord ((TableSpec spec) as ts) =
    spec.upsertRecord ts


getCloneRecord : TableSpec a -> a -> Flow Model ()
getCloneRecord ((TableSpec spec) as ts) =
    spec.cloneRecord ts
