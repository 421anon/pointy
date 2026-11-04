{-# LANGUAGE OverloadedStrings #-}

module Agent.Router (JudgedMessage (..), Route (..), routeTurn) where

import Agent.Exchange (Exchange (..), exchangeReply, turnExchanges)
import Agent.Model (AgentRole (..))
import Agent.Policy (RoutingChoice (..), routingChoices, routingInstructions)
import Agent.Session (AgentTurn (..), ViewedArtifact (..), isChangesetLifecycleTurn, listTurnsWithLogs)
import Config (Config (..), RouterConfig (..), loadConfig, resolveConfigPath)
import Data.Aeson (Key, Value, object, (.=))
import Data.List (find, sortOn, stripPrefix)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import ProviderHttp (choiceRequest)
import System.Exit (ExitCode (..))
import UserRepo (runGitIn, userRepoPath)

data JudgedMessage = JudgedMessage
    { judgedText :: Text
    , judgedImages :: Int
    , judgedAreaSelections :: Int
    , judgedArtifact :: Maybe ViewedArtifact
    }

data Route
    = SoleCoder
    | Routed AgentRole (Maybe Value) (Maybe Text)

routeTurn :: AgentTurn -> Maybe JudgedMessage -> IO Route
routeTurn turn mMessage = do
    config <- resolveConfigPath >>= loadConfig
    case (configRouter config, mMessage) of
        (Nothing, _) -> pure SoleCoder
        (Just _, Nothing) -> pure (Routed Coder Nothing Nothing)
        (Just router, Just message) -> do
            context <- messageContext message
            case Map.lookup (routerProvider router) (configProviders config) of
                Nothing -> pure (failed context ("the provider \"" <> T.unpack (routerProvider router) <> "\" is not configured"))
                Just provider -> do
                    earlier <- earlierExchanges turn
                    let conversation = map exchangeJson earlier ++ [messageJson (judgedText message) context]
                    either (failed context) (routed context) <$> choiceRequest provider (routerModel router) (object ["conversation" .= conversation]) routingInstructions criteria
  where
    criteria = [(choiceLabel choice, choiceMeaning choice) | choice <- routingChoices]
    routed context label = case find ((== label) . choiceLabel) routingChoices of
        Just choice -> Routed (choiceRole choice) context Nothing
        Nothing -> failed context ("the router chose \"" <> T.unpack label <> "\"")
    failed context reason = Routed Coder context (Just ("Routing failed, so the coder takes this turn: " <> T.pack reason))

messageContext :: JudgedMessage -> IO (Maybe Value)
messageContext message = do
    viewing <- traverse viewingContext (judgedArtifact message)
    let fields =
            ["images" .= judgedImages message | judgedImages message > 0]
                ++ ["areaSelections" .= judgedAreaSelections message | judgedAreaSelections message > 0]
                ++ ["viewing" .= value | Just value <- [viewing]]
    pure (if null fields then Nothing else Just (object fields))

viewingContext :: ViewedArtifact -> IO Value
viewingContext artifact = do
    repo <- userRepoPath
    let stepId = show (viewedArtifactStepId artifact)
        commit = T.unpack (viewedArtifactCommit artifact)
        sourceDir = "srcFiles/" ++ stepId ++ "/"
    (definitionCode, definition, _) <- runGitIn repo ["show", commit ++ ":steps/" ++ stepId ++ ".nix"]
    (listingCode, listing, _) <- runGitIn repo ["ls-tree", "-r", "--name-only", commit, "--", sourceDir]
    pure $
        object $
            [ "step" .= viewedArtifactStepId artifact
            , "file" .= T.intercalate "/" (viewedArtifactPath artifact)
            ]
                ++ ["stepDefinition" .= T.pack definition | definitionCode == ExitSuccess]
                ++ ["sourceFiles" .= [fromMaybe path (stripPrefix sourceDir path) | path <- lines listing] | listingCode == ExitSuccess]

earlierExchanges :: AgentTurn -> IO [Exchange]
earlierExchanges current = do
    turns <- listTurnsWithLogs (turnSessionId current)
    pure
        [ exchange
        | turn <- sortOn turnStartedAt turns
        , turnId turn /= turnId current
        , turnStartedAt turn <= turnStartedAt current
        , not (isChangesetLifecycleTurn turn)
        , exchange <- turnExchanges turn
        ]

exchangeJson :: Exchange -> Value
exchangeJson exchange =
    object $
        messageFields (exchangeMessage exchange) (exchangeContext exchange)
            ++ ["reply" .= exchangeReply exchange]
            ++ [ "routedTo" .= choiceLabel choice
               | Just role <- [exchangeRole exchange]
               , Just choice <- [find ((== role) . choiceRole) routingChoices]
               ]

messageJson :: Text -> Maybe Value -> Value
messageJson message context = object (messageFields message context)

messageFields :: Text -> Maybe Value -> [(Key, Value)]
messageFields message context = ("message" .= message) : ["context" .= value | Just value <- [context]]
