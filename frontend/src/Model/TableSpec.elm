module Model.TableSpec exposing
    ( StepSpec
    , TableSpec(..)
    , getApiPath
    , getCloneRecord
    , getDefaultRecord
    , getDescription
    , getDirectoryView
    , getDisplayName
    , getEncodeRecord
    , getLens
    , getName
    , getShareable
    , getSrcFilesView
    , getStatus
    , getTag
    , getUpsertRecord
    )

import Accessors exposing (Traversal)
import Api.ApiData as ApiData exposing (ApiData)
import Extra.Accessors exposing (A_Traversal, remkT)
import Flow exposing (Flow)
import Json.Decode
import Json.Encode
import Model.Core exposing (DirectoryFolder, Model, Status(..), StepRecord, Table, TableTag)


type TableSpec a
    = TableSpec
        { tag : TableTag
        , name : String
        , lens : A_Traversal Model (Table a)
        , status : a -> ApiData Status
        , directoryView : a -> Maybe DirectoryFolder
        , srcFilesView : a -> Maybe DirectoryFolder
        , encodeRecord : a -> Json.Encode.Value
        , decodeRecord : Json.Decode.Decoder a
        , defaultRecord : a
        , apiPath : String
        , displayName : String
        , description : Maybe String
        , upsertRecord : TableSpec a -> Flow Model ()
        , cloneRecord : TableSpec a -> a -> Flow Model ()
        }


type alias StepSpec =
    TableSpec StepRecord


getTag : TableSpec a -> TableTag
getTag (TableSpec spec) =
    spec.tag


getName : TableSpec a -> String
getName (TableSpec spec) =
    spec.name


getDisplayName : TableSpec a -> String
getDisplayName (TableSpec spec) =
    spec.displayName


getDescription : TableSpec a -> Maybe String
getDescription (TableSpec spec) =
    spec.description


getLens : TableSpec a -> Traversal Model (Table a) x y
getLens (TableSpec spec) =
    remkT spec.lens


getStatus : TableSpec a -> a -> ApiData Status
getStatus (TableSpec spec) =
    spec.status


getShareable : TableSpec a -> a -> Bool
getShareable (TableSpec spec) =
    spec.status
        >> ApiData.toMaybe
        >> Maybe.map (\status_ -> status_ == StatusSuccess || status_ == StatusRunning)
        >> Maybe.withDefault False


getDirectoryView : TableSpec a -> a -> Maybe DirectoryFolder
getDirectoryView (TableSpec spec) =
    spec.directoryView


getSrcFilesView : TableSpec a -> a -> Maybe DirectoryFolder
getSrcFilesView (TableSpec spec) =
    spec.srcFilesView


getEncodeRecord : TableSpec a -> a -> Json.Encode.Value
getEncodeRecord (TableSpec spec) record =
    spec.encodeRecord record


getDefaultRecord : TableSpec a -> a
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
