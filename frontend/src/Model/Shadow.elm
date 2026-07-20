module Model.Shadow exposing (..)

import Accessors exposing (Prism, prism)
import Basics.Extra exposing (uncurry)
import Dict exposing (Dict)


type StepArgType
    = TString TStringDisplay (Maybe String)
    | TStep (Maybe (List String))
    | TUploadHash
    | TList StepArgType
    | TRecord (Dict String ArgType)
    | TEnum (List String) (Dict String String)


type TStringDisplay
    = TextField
    | TextArea
    | Command String
    | Code String


type StepArgValue
    = TStringValue String
    | TStepValue Int
    | TUploadHashValue String
    | TListValue (List StepArgValue)
    | TRecordValue (Dict String StepArgValue)
    | TEnumValue String


tStringValue : Prism ls StepArgValue String x y
tStringValue =
    prism ">TStringValue"
        TStringValue
        (\stepArgVal ->
            case stepArgVal of
                TStringValue val ->
                    Ok val

                _ ->
                    Err stepArgVal
        )


tStepId : Prism ls StepArgValue Int x y
tStepId =
    prism ">TStepId"
        TStepValue
        (\stepArgVal ->
            case stepArgVal of
                TStepValue val ->
                    Ok val

                _ ->
                    Err stepArgVal
        )


tListValue : Prism ls StepArgValue (List StepArgValue) x y
tListValue =
    prism ">TListValue"
        TListValue
        (\stepArgVal ->
            case stepArgVal of
                TListValue val ->
                    Ok val

                _ ->
                    Err stepArgVal
        )


tRecordValue : Prism ls StepArgValue (Dict String StepArgValue) x y
tRecordValue =
    prism ">TRecordValue"
        TRecordValue
        (\stepArgVal ->
            case stepArgVal of
                TRecordValue val ->
                    Ok val

                _ ->
                    Err stepArgVal
        )


tEnumValue : Prism ls StepArgValue String x y
tEnumValue =
    prism ">TEnumValue"
        TEnumValue
        (\stepArgVal ->
            case stepArgVal of
                TEnumValue val ->
                    Ok val

                _ ->
                    Err stepArgVal
        )


type alias ArgType =
    { description : String, type_ : StepArgType, displayName : Maybe String }


type StepType
    = FileUpload (Maybe (List String))
    | Derivation (Dict String ArgType) WithSrcFiles
    | Download


type WithSrcFiles
    = WithSrcFiles
    | WithoutSrcFiles


downloadArgs : Dict String ArgType
downloadArgs =
    Dict.singleton "url"
        { description = "URL to fetch into the Nix store"
        , type_ = TString TextField Nothing
        , displayName = Just "URL"
        }


derivation : Prism ls StepType ( Dict String ArgType, WithSrcFiles ) x y
derivation =
    prism ">Derivation"
        (uncurry Derivation)
        (\stepType ->
            case stepType of
                Derivation args src ->
                    Ok ( args, src )

                _ ->
                    Err stepType
        )


type alias StepConfigEntry =
    { stepType : StepType
    , sortKey : Maybe Int
    , displayName : Maybe String
    , description : Maybe String
    }


type alias StepConfig =
    Dict String StepConfigEntry


type alias Preset =
    { displayName : String
    , description : Maybe String
    , sortKey : Maybe Int
    , templates : List String
    }


type alias Presets =
    Dict String Preset
