{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import BuildStatus (StepPaths (..))
import BuiltSteps (LastBuiltVersion (..), recordBuiltStatuses, resolveLastBuilt)
import Bus (StatusEvent (..), subscribe)
import CertificateStore (storeCertificate, storeOutput)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (TChan, atomically, readTChan)
import Control.Monad (forM_, unless)
import Data.Aeson (Value (Null), encode, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Char (isDigit, isSpace)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import EffectRunner (Runner (..), installRunner, runAppEffects)
import Effectful (Eff, IOE, liftIO, runEff, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effects (AppEffects, Eval (..))
import Fixture.Document (FixtureDocument (..))
import Handlers.LastBuilt (lastBuiltVersion)
import Interpreters.Fixture (FixtureState, newFixtureState, runFixture, runInferenceFixture, runNixFixture, runSlurmFixture)
import NixEvaluator (RepoSource (..))
import ProjectFiles (stepFilePath)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getEnvironment, setEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)
import System.Timeout (timeout)

main :: IO ()
main = do
    nearestBuiltAncestorWins
    olderViewNeverSeesLaterBuild
    revertResolvesToRevertingCommit
    collectedBuildsAreSkipped
    unrecordedStepResolvesToNothing
    backfillFindsEarlierBuildsOnce

nearestBuiltAncestorWins :: IO ()
nearestBuiltAncestorWins =
    withUserRepo "nearest" $ \commitAt -> do
        a1 <- commitAt 1 []
        b1 <- commitAt 2 []
        c1 <- commitAt 3 []
        let certified = stepPaths 101 '1'
            uncertified = stepPaths 101 '2'
        observe a1 101 certified ("success", Nothing)
        observe b1 101 uncertified ("built-not-certified", Nothing)
        observe c1 101 (stepPaths 101 '3') ("not-started", Nothing)
        found <- resolveAt (bothPaths certified ++ [stepOutput uncertified]) c1 101
        assertEqual "the nearest built ancestor wins" (Map.singleton 101 (LastBuiltVersion b1 (committedAt 2))) found

olderViewNeverSeesLaterBuild :: IO ()
olderViewNeverSeesLaterBuild =
    withUserRepo "older-view" $ \commitAt -> do
        a1 <- commitAt 1 []
        b1 <- commitAt 2 []
        c1 <- commitAt 3 []
        let earlier = stepPaths 102 '1'
            later = stepPaths 102 '3'
        observe a1 102 earlier ("success", Nothing)
        observe b1 102 (stepPaths 102 '2') ("not-started", Nothing)
        observe c1 102 later ("success", Nothing)
        found <- resolveAt (bothPaths earlier ++ bothPaths later) b1 102
        assertEqual "an older view never sees a later build" (Map.singleton 102 (LastBuiltVersion a1 (committedAt 1))) found

revertResolvesToRevertingCommit :: IO ()
revertResolvesToRevertingCommit =
    withUserRepo "revert" $ \commitAt -> do
        a1 <- commitAt 1 []
        b1 <- commitAt 2 []
        c1 <- commitAt 3 []
        d1 <- commitAt 4 []
        let original = stepPaths 103 '1'
            changed = stepPaths 103 '2'
        observe a1 103 original ("success", Nothing)
        observe b1 103 changed ("success", Nothing)
        observe c1 103 original ("success", Nothing)
        observe d1 103 (stepPaths 103 '4') ("not-started", Nothing)
        found <- resolveAt (bothPaths original ++ bothPaths changed) d1 103
        assertEqual "a revert resolves to the reverting commit" (Map.singleton 103 (LastBuiltVersion c1 (committedAt 3))) found

collectedBuildsAreSkipped :: IO ()
collectedBuildsAreSkipped =
    withUserRepo "collected" $ \commitAt -> do
        a1 <- commitAt 1 []
        b1 <- commitAt 2 []
        c1 <- commitAt 3 []
        let kept = stepPaths 104 '1'
        observe a1 104 kept ("success", Nothing)
        observe b1 104 (stepPaths 104 '2') ("success", Nothing)
        observe c1 104 (stepPaths 104 '3') ("not-started", Nothing)
        found <- resolveAt (bothPaths kept) c1 104
        assertEqual "garbage-collected builds are skipped" (Map.singleton 104 (LastBuiltVersion a1 (committedAt 1))) found

unrecordedStepResolvesToNothing :: IO ()
unrecordedStepResolvesToNothing =
    withUserRepo "unrecorded" $ \commitAt -> do
        a1 <- commitAt 1 []
        b1 <- commitAt 2 []
        let neighbour = stepPaths 106 '1'
        observe a1 106 neighbour ("success", Nothing)
        observe a1 105 (stepPaths 105 '1') ("failure", Just "error: builder failed")
        observe b1 105 (stepPaths 105 '2') ("not-started", Nothing)
        found <- resolveAt (bothPaths neighbour) b1 105
        assertEqual "a step never recorded resolves to nothing" Map.empty found

backfillFindsEarlierBuildsOnce :: IO ()
backfillFindsEarlierBuildsOnce =
    withUserRepo "backfill" $ \commitAt -> do
        a1 <- commitAt 1 [oldStep]
        b1 <- commitAt 2 [neverBuiltStep]
        c1 <- commitAt 3 []
        forM_ [(revision, version, sid) | (revision, version) <- [(a1, 'a'), (b1, 'b'), (c1, 'c')], sid <- [oldStep, neverBuiltStep]] $ \(revision, version, sid) -> do
            storeCertificate (stepKey revision sid) (stepCertificate (stepPaths sid version))
            unless (revision == a1) $ storeOutput (stepKey revision sid) (stepOutput (stepPaths sid version))
        state <- newFixtureState document{documentValidPaths = [stepCertificate (stepPaths oldStep 'a')]}
        evaluations <- newIORef []
        installRunner (Runner (runWithStepKeys state evaluations))
        events <- subscribe

        neverBuilt <- runAppEffects (lastBuiltVersion neverBuiltStep c1)
        assertEqual "a step built nowhere has no last built version" Nothing neverBuilt
        unrecorded <- runAppEffects (lastBuiltVersion oldStep c1)
        assertEqual "the answer does not wait for the history search" Nothing unrecorded

        announced <- timeout 10000000 (lastBuiltAnnouncedAt events c1)
        assertEqual "the history search announces the earlier build" (Just (Map.singleton oldStep (LastBuiltVersion a1 (committedAt 1)))) announced
        searched <- readIORef evaluations
        assertEqual
            "one search evaluates each ancestor once for all the steps it defines, and never the viewed revision"
            (sort [(a1, [oldStep]), (b1, [oldStep, neverBuiltStep])])
            (sort [(revision, sort steps) | (revision, steps) <- searched])

        recorded <- runAppEffects (lastBuiltVersion oldStep c1)
        assertEqual "the found build answers at once" (Just (LastBuiltVersion a1 (committedAt 1))) recorded
        stillNeverBuilt <- runAppEffects (lastBuiltVersion neverBuiltStep c1)
        assertEqual "a searched step stays without a last built version" Nothing stillNeverBuilt
        elsewhere <- runAppEffects (lastBuiltVersion neverBuiltStep b1)
        assertEqual "a searched step is not searched again from another revision" Nothing elsewhere
        threadDelay 2000000
        settled <- readIORef evaluations
        assertEqual "later requests evaluate nothing" searched settled

oldStep :: Int
oldStep = 3

neverBuiltStep :: Int
neverBuiltStep = 5

stepKey :: Text -> Int -> Text
stepKey revision sid = T.pack (show sid) <> "-" <> revision

lastBuiltAnnouncedAt :: TChan StatusEvent -> Text -> IO (Map Int LastBuiltVersion)
lastBuiltAnnouncedAt events revision =
    atomically (readTChan events) >>= \case
        LastBuiltEvent announced versions | announced == revision -> pure versions
        _ -> lastBuiltAnnouncedAt events revision

runWithStepKeys :: FixtureState -> IORef [(Text, [Int])] -> Eff AppEffects a -> IO a
runWithStepKeys state evaluations =
    runEff . runInferenceFixture . runSlurmFixture state . runNixFixture state . runStepKeys evaluations

runStepKeys :: (IOE :> es) => IORef [(Text, [Int])] -> Eff (Eval : es) a -> Eff es a
runStepKeys evaluations = interpret $ \_ -> \case
    EvalJsonApply source applyExpr "#pointy" -> liftIO (stepKeysAt evaluations (revisionOf source) (quotedSteps applyExpr))
    EvalJsonApply _ applyExpr attr -> unexpectedEvaluation (applyExpr ++ " on " ++ attr)
    EvalJson _ attr -> unexpectedEvaluation attr
    EvalRaw _ attr -> unexpectedEvaluation attr
    EvalImpure expression -> unexpectedEvaluation expression

stepKeysAt :: IORef [(Text, [Int])] -> Text -> [Int] -> IO (Either String String)
stepKeysAt evaluations revision steps = do
    atomicModifyIORef' evaluations (\logged -> ((revision, steps) : logged, ()))
    pure (Right (LBS8.unpack (encode (object ["version" .= (1 :: Int), "value" .= Map.fromList [(sid, stepKey revision sid) | sid <- steps]]))))

unexpectedEvaluation :: (IOE :> es) => String -> Eff es (Either String String)
unexpectedEvaluation request = liftIO (fail ("unexpected evaluation of " ++ request))

quotedSteps :: String -> [Int]
quotedSteps expression = [read digits | '"' : quoted <- words expression, (digits@(_ : _), "\"") <- [span isDigit quoted]]

revisionOf :: RepoSource -> Text
revisionOf (RepoRef reference) = T.takeWhile (/= '&') (snd (T.breakOnEnd "?rev=" (T.pack reference)))
revisionOf (RepoWorktree worktree) = T.pack worktree

observe :: Text -> Int -> StepPaths -> (Text, Maybe Text) -> IO ()
observe commit sid paths status = recordBuiltStatuses commit (Map.singleton sid paths) (Map.singleton sid status)

resolveAt :: [FilePath] -> Text -> Int -> IO (Map Int LastBuiltVersion)
resolveAt valid revision sid = do
    state <- newFixtureState document{documentValidPaths = valid}
    runFixture state (resolveLastBuilt revision [sid])

stepPaths :: Int -> Char -> StepPaths
stepPaths sid version = StepPaths (storePath sid version "certificate") (storePath sid version "output")

storePath :: Int -> Char -> String -> FilePath
storePath sid version name = "/nix/store/" ++ replicate 32 version ++ "-pointy-step-" ++ show sid ++ "-" ++ name

bothPaths :: StepPaths -> [FilePath]
bothPaths paths = [stepCertificate paths, stepOutput paths]

withUserRepo :: String -> ((Integer -> [Int] -> IO Text) -> IO a) -> IO a
withUserRepo label action =
    withSystemTempDirectory ("pointy-last-built-" ++ label) $ \home -> do
        setEnv "HOME" home
        let repo = home </> "user-repo.git"
        _ <- git [] ["init", "--quiet", repo]
        createDirectoryIfMissing True (repo </> "steps")
        action (commitIn repo label)

commitIn :: FilePath -> String -> Integer -> [Int] -> IO Text
commitIn repo label hour createdSteps = do
    let date = show (hourSeconds hour) ++ " +0000"
    forM_ createdSteps $ \sid -> writeFile (stepFilePath repo sid) "{ }\n"
    _ <- git [] ["-C", repo, "add", "--all"]
    _ <-
        git
            [("GIT_AUTHOR_DATE", date), ("GIT_COMMITTER_DATE", date)]
            ["-c", "user.name=Pointy Test", "-c", "user.email=pointy-test@example.invalid", "-c", "commit.gpgsign=false", "-C", repo, "commit", "--quiet", "--allow-empty", "--message", label ++ " " ++ show hour]
    pack . filter (not . isSpace) <$> git [] ["-C", repo, "rev-parse", "HEAD"]

git :: [(String, String)] -> [String] -> IO String
git overrides args = do
    environment <- getEnvironment
    (code, out, err) <- readCreateProcessWithExitCode (proc "git" args){env = Just (overrides ++ environment)} ""
    unless (code == ExitSuccess) $ fail ("git " ++ unwords args ++ " failed: " ++ err)
    pure out

hourSeconds :: Integer -> Integer
hourSeconds hour = 1790000000 + 3600 * hour

committedAt :: Integer -> UTCTime
committedAt = posixSecondsToUTCTime . fromInteger . hourSeconds

document :: FixtureDocument
document =
    FixtureDocument
        { documentBranch = "main"
        , documentJson = Map.empty
        , documentRaw = Map.empty
        , documentPresets = Null
        , documentOutPaths = Map.empty
        , documentCertificates = Map.empty
        , documentExtrasOutPaths = Map.empty
        , documentNotices = Map.empty
        , documentReviews = Map.empty
        , documentProjectStepIds = Map.empty
        , documentValidPaths = []
        , documentDerivations = Map.empty
        , documentLogs = Map.empty
        , documentReferences = Map.empty
        , documentOutputs = Map.empty
        }

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual =
    unless (actual == expected) $
        fail (label ++ ": expected " ++ show expected ++ ", got " ++ show actual)
