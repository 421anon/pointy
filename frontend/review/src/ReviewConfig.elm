module ReviewConfig exposing (config)

import NoConfusingPrefixOperator
import NoDuplicatePorts
import NoEmptyText
import NoEtaReducibleLambdas
import NoExposingEverything
import NoInconsistentAliases
import NoLeftPizza
import NoMissingTypeAnnotation
import NoModuleOnExposedNames
import NoPrematureLetComputation
import NoRedundantlyQualifiedType
import NoSimpleLetBody
import NoUnmatchedUnit
import NoUnsafePorts
import NoUnused.CustomTypeConstructorArgs
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Modules
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import NoUnusedPorts
import Review.Rule exposing (Rule, ignoreErrorsForFiles)
import Simplify
import UseCamelCase


config : List Rule
config =
    [ NoUnused.CustomTypeConstructors.rule [] |> ignoreErrorsForFiles [ "src/Keyboard.elm", "src/Extra/Events.elm" ]
    , NoUnused.CustomTypeConstructorArgs.rule
    , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule |> ignoreErrorsForFiles [ "src/Keyboard.elm", "src/Extra/Events.elm", "src/Extra/IO.elm", "src/Extra/Accessors.elm" ]
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoUnused.Variables.rule
    , NoLeftPizza.rule NoLeftPizza.Redundant
    , Simplify.rule Simplify.defaults
    , NoPrematureLetComputation.rule
    , NoMissingTypeAnnotation.rule
    , NoRedundantlyQualifiedType.rule
    , NoSimpleLetBody.rule
    , UseCamelCase.rule UseCamelCase.default |> ignoreErrorsForFiles [ "src/Extra/Accessors.elm" ]
    , NoInconsistentAliases.config
        [ ( "Html.Extra", "Html" )
        , ( "List.Extra", "List" )
        , ( "Maybe.Extra", "Maybe" )
        ]
        |> NoInconsistentAliases.noMissingAliases
        |> NoInconsistentAliases.rule
    , NoModuleOnExposedNames.rule
    , NoUnmatchedUnit.rule
    , NoEmptyText.rule
    , NoEtaReducibleLambdas.rule
        { lambdaReduceStrategy = NoEtaReducibleLambdas.AlwaysRemoveLambdaWhenPossible
        , argumentNamePredicate = always True
        }
    , NoDuplicatePorts.rule
    , NoUnusedPorts.rule
    ]
