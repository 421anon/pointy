module Model.Shadow exposing (..)

import Accessors exposing (Prism, prism)
import Basics.Extra exposing (uncurry)
import Dict exposing (Dict)


type StepArgType
    = TString TStringDisplay
    | TStep (Maybe (List String))
    | TUploadHash
    | TList StepArgType


type TStringDisplay
    = TextField
    | TextArea
    | Command String


type StepArgValue
    = TStringValue String
    | TStepValue Int
    | TUploadHashValue String
    | TListValue (List StepArgValue)


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


type alias ArgType =
    { description : String, type_ : StepArgType }


type StepType
    = FileUpload (Maybe (List String))
    | Derivation (Dict String ArgType) WithSrcFiles


type WithSrcFiles
    = WithSrcFiles
    | WithoutSrcFiles


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
    }


type alias StepConfig =
    Dict String StepConfigEntry
