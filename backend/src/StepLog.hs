{-# LANGUAGE OverloadedStrings #-}

module StepLog (
    BuildRef (..),
    BuildStage (..),
    StepBuild (..),
    StepLogView (..),
    stepLogView,
    stepLogWithoutLog,
) where

import BuildLog (ResolvedLog (..))
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), decodeStrict, object, withObject, (.:), (.:?), (.=))
import Data.Aeson.Types (parseMaybe)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.FilePath (takeFileName)
import Text.Read (readMaybe)

data BuildStage = StageBuild | StageTypeCheck
    deriving (Eq, Show)

data BuildRef
    = StepRef Int Text BuildStage
    | PackageRef Text
    deriving (Eq, Show)

data LogSection = LogSection
    { sectionPhase :: Maybe Text
    , sectionText :: Text
    }
    deriving (Eq, Show)

data StepLogView = StepLogView
    { viewFailed :: BuildRef
    , viewVia :: [BuildRef]
    , viewReason :: Maybe Text
    , viewOutput :: Text
    , viewLog :: Text
    }
    deriving (Eq, Show)

data StepBuild = StepBuild
    { stepBuildId :: Int
    , stepBuildName :: Text
    , stepBuildDrv :: FilePath
    , stepCheckDrv :: Maybe FilePath
    , stepBuildPhases :: [Text]
    }
    deriving (Eq, Show)

instance FromJSON StepBuild where
    parseJSON = withObject "step build" $ \o -> do
        rawId <- o .: "id"
        stepId <- maybe (fail ("step id is not a number: " ++ rawId)) pure (readMaybe rawId)
        StepBuild stepId <$> o .: "name" <*> o .: "build" <*> o .:? "check" <*> o .: "phases"

instance ToJSON BuildRef where
    toJSON (StepRef stepId name stage) =
        object ["kind" .= ("step" :: Text), "stepId" .= stepId, "name" .= name, "stage" .= stageName stage]
      where
        stageName StageBuild = "build" :: Text
        stageName StageTypeCheck = "typeCheck"
    toJSON (PackageRef name) = object ["kind" .= ("package" :: Text), "name" .= name]

instance ToJSON StepLogView where
    toJSON view =
        object
            [ "failed" .= viewFailed view
            , "via" .= viewVia view
            , "reason" .= viewReason view
            , "output" .= viewOutput view
            , "log" .= viewLog view
            ]

stepLogView :: Int -> [StepBuild] -> Maybe Text -> ResolvedLog -> StepLogView
stepLogView requested builds reason resolved =
    StepLogView
        { viewFailed = failedRef
        , viewVia = filter (not . isRequested) (map (fst . describe) (resolvedVia resolved))
        , viewReason = reason
        , viewOutput = shownOutput failedPhases (logSections (resolvedLog resolved))
        , viewLog = TE.decodeUtf8Lenient (resolvedLog resolved)
        }
  where
    (failedRef, failedPhases) = describe (resolvedDrv resolved)
    describe drv = Map.findWithDefault (PackageRef (derivationName drv), Nothing) drv (buildIndex builds)
    isRequested (StepRef stepId _ _) = stepId == requested
    isRequested (PackageRef _) = False

stepLogWithoutLog :: Int -> [StepBuild] -> Text -> StepLogView
stepLogWithoutLog requested builds reason =
    StepLogView
        { viewFailed = requestedRef
        , viewVia = []
        , viewReason = Just reason
        , viewOutput = ""
        , viewLog = ""
        }
  where
    requestedRef =
        fromMaybe (StepRef requested "" StageBuild) $
            listToMaybe [StepRef (stepBuildId build) (stepBuildName build) StageBuild | build <- builds, stepBuildId build == requested]

buildIndex :: [StepBuild] -> Map FilePath (BuildRef, Maybe [Text])
buildIndex builds =
    Map.fromList $
        concat
            [ (stepBuildDrv build, (StepRef (stepBuildId build) (stepBuildName build) StageBuild, Just (stepBuildPhases build)))
                : [(check, (StepRef (stepBuildId build) (stepBuildName build) StageTypeCheck, Nothing)) | Just check <- [stepCheckDrv build]]
            | build <- builds
            ]

derivationName :: FilePath -> Text
derivationName drv =
    let file = T.pack (takeFileName drv)
        named = T.drop 1 (T.dropWhile (/= '-') file)
     in fromMaybe named (T.stripSuffix ".drv" named)

shownOutput :: Maybe [Text] -> [LogSection] -> Text
shownOutput templatePhases sections =
    T.intercalate "\n" (filter (not . T.null . T.strip) (map sectionText (filter shown sections)))
  where
    shown section = case (templatePhases, sectionPhase section) of
        (Nothing, _) -> True
        (Just phases, Just phase) -> phase `elem` phases
        (Just _, Nothing) -> all ((== Nothing) . sectionPhase) sections

data LogLine
    = PhaseStart Text
    | ProtocolMessage
    | Output ByteString

logSections :: ByteString -> [LogSection]
logSections logBytes = reverse (close (foldl' advance (Nothing, [], []) (map classify (BC.lines logBytes))))
  where
    advance (phase, acc, closed) (PhaseStart next) = (Just next, [], close (phase, dropHeader next acc, closed))
    advance current ProtocolMessage = current
    advance (phase, acc, closed) (Output line) = (phase, line : acc, closed)
    close (phase, acc, closed) = LogSection phase (TE.decodeUtf8Lenient (BC.intercalate "\n" (reverse acc))) : closed

classify :: ByteString -> LogLine
classify line = case BS.stripPrefix "@nix " line >>= decodeStrict of
    Just (Object o) -> case (parseMaybe (.: "action") o :: Maybe Text) of
        Just "setPhase" -> maybe ProtocolMessage PhaseStart (parseMaybe (.: "phase") o)
        _ -> ProtocolMessage
    _ -> Output line

dropHeader :: Text -> [ByteString] -> [ByteString]
dropHeader phase (latest : earlier)
    | latest == TE.encodeUtf8 ("Running phase: " <> phase) = earlier
dropHeader _ acc = acc
