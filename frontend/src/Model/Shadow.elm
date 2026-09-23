module Model.Shadow exposing (..)

import Accessors exposing (Prism, prism)
import Basics.Extra exposing (uncurry)
import Dict exposing (Dict)


type Widget
    = WText (Maybe String)
    | WTextarea
    | WCode String
    | WCommand String
    | WNumber
    | WCheckbox
    | WSelect (List ( String, String ))
    | WTokens (Maybe String)
    | WList Widget
    | WStep Artifact
    | WSteps Artifact
    | WRecord (List Field)
    | WDatetime


type alias Artifact =
    { accepts : Maybe (List String)
    , proven : Maybe (List String)
    , create : Bool
    }


type alias Field =
    { name : String
    , label : Maybe String
    , help : String
    , readOnly : Bool
    , argsPath : Maybe (List String)
    , widget : Widget
    }


type StepArgValue
    = TStringValue String
    | TIntValue Int
    | TBoolValue Bool
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


tIntValue : Prism ls StepArgValue Int x y
tIntValue =
    prism ">TIntValue"
        TIntValue
        (\stepArgVal ->
            case stepArgVal of
                TIntValue val ->
                    Ok val

                _ ->
                    Err stepArgVal
        )


tBoolValue : Prism ls StepArgValue Bool x y
tBoolValue =
    prism ">TBoolValue"
        TBoolValue
        (\stepArgVal ->
            case stepArgVal of
                TBoolValue val ->
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


type StepType
    = FileUpload (Maybe (List String))
    | Derivation (List Field) WithSrcFiles
    | Download (List Field)


type WithSrcFiles
    = WithSrcFiles
    | WithoutSrcFiles


derivation : Prism ls StepType ( List Field, WithSrcFiles ) x y
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
    , icon : Maybe String
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
