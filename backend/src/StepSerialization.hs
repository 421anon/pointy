{-# LANGUAGE OverloadedStrings #-}

module StepSerialization (evaluateStepJsonToNix) where

import Data.Aeson (Value (..), eitherDecodeStrict', withObject, (.:))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser, parseEither)
import Data.Fix (Fix (..), foldFix)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Nix (nixEvalExpr, withNixContext)
import Nix.Expr.Shorthands (mkIndentedStr, mkStr, mkSym, (@.), (@@))
import Nix.Expr.Types (Antiquoted (..), Binding (..), NExpr, NExprF (..), NKeyName (..), NString (..), VarName (..))
import Nix.Normal (normalForm)
import Nix.Options (defaultOptions)
import Nix.Pretty (exprFNixDoc, getDoc, simpleExpr, valueToExpr)
import Nix.Standard (runWithBasicEffectsIO)
import NixUtils (sortAttrSet)
import Prettyprinter (defaultLayoutOptions, hardline, layoutPretty, pretty)
import Prettyprinter.Render.Text (renderStrict)

type ArgShapes = KM.KeyMap ArgShape

data ArgShape
    = Keep
    | Multiline
    | ListOf ArgShape
    | RecordOf ArgShapes

evaluateStepJsonToNix :: Value -> T.Text -> IO (Either String T.Text)
evaluateStepJsonToNix config stepJson =
    case eitherDecodeStrict' (TE.encodeUtf8 stepJson) of
        Left err -> pure $ Left $ "Invalid step request JSON: " ++ err
        Right step ->
            case parseEither (parseStepShapes config) step of
                Left err -> pure $ Left err
                Right shapes -> do
                    expression <- jsonToNixExpr stepJson
                    let rewritten = maybe expression (`rewriteStepArgs` expression) shapes
                    pure $ Right $ renderStepExpr $ sortAttrSet rewritten

jsonToNixExpr :: T.Text -> IO NExpr
jsonToNixExpr jsonText = do
    let expression = mkSym "builtins" @. "fromJSON" @@ mkStr jsonText
        options = defaultOptions $ posixSecondsToUTCTime 0
    runWithBasicEffectsIO options $ withNixContext Nothing $ do
        value <- nixEvalExpr Nothing expression
        valueToExpr <$> normalForm value

parseStepShapes :: Value -> Value -> Parser (Maybe ArgShapes)
parseStepShapes config = withObject "step request" $ \step -> do
    stepType <- step .: "type"
    entry <- withObject "step config" (.: Key.fromText stepType) config
    stepTypeValue <- withObject "step config entry" (.: "type") entry
    withObject "step type" (\types -> traverse parseDerivation $ KM.lookup "derivation" types) stepTypeValue

parseDerivation :: Value -> Parser ArgShapes
parseDerivation = withObject "derivation step type" $ \derivation ->
    derivation .: "args" >>= parseArgShapes "derivation arguments"

parseArgShapes :: String -> Value -> Parser ArgShapes
parseArgShapes label = withObject label $ traverse parseArgShape

parseArgShape :: Value -> Parser ArgShape
parseArgShape = withObject "argument config" $ \arg -> arg .: "type" >>= parseArgType

parseArgType :: Value -> Parser ArgShape
parseArgType = withObject "argument type" $ \types ->
    case KM.lookup "string" types of
        Just string -> parseStringShape string
        Nothing
            | KM.member "enum" types || KM.member "step" types -> pure Keep
            | Just item <- KM.lookup "list" types -> ListOf <$> parseArgType item
            | Just record <- KM.lookup "record" types -> parseRecordShape record
            | otherwise -> pure Keep

parseStringShape :: Value -> Parser ArgShape
parseStringShape = withObject "string argument type" $ \string -> do
    display <- string .: "display"
    case display of
        Object options ->
            case KM.lookup "code" options of
                Just code -> Multiline <$ (withObject "code display" (.: "language") code :: Parser T.Text)
                Nothing
                    | KM.member "textarea" options -> pure Multiline
                    | otherwise -> pure Keep
        _ -> pure Keep

parseRecordShape :: Value -> Parser ArgShape
parseRecordShape = withObject "record argument type" $ \record ->
    record .: "fields" >>= fmap RecordOf . parseArgShapes "record argument fields"

rewriteStepArgs :: ArgShapes -> NExpr -> NExpr
rewriteStepArgs shapes (Fix (NSet recursivity bindings)) =
    Fix $ NSet recursivity $ map rewriteBinding bindings
  where
    rewriteBinding binding@(NamedVar path value position)
        | staticKey path == Just "args" = NamedVar path (rewriteRecord shapes value) position
        | otherwise = binding
    rewriteBinding binding = binding
rewriteStepArgs _ expression = expression

rewriteByShape :: ArgShape -> NExpr -> NExpr
rewriteByShape Keep expression = expression
rewriteByShape Multiline expression = rewriteString expression
rewriteByShape (ListOf itemShape) (Fix (NList items)) = Fix $ NList $ map (rewriteByShape itemShape) items
rewriteByShape (ListOf _) expression = expression
rewriteByShape (RecordOf fields) expression = rewriteRecord fields expression

rewriteRecord :: ArgShapes -> NExpr -> NExpr
rewriteRecord shapes (Fix (NSet recursivity bindings)) =
    Fix $ NSet recursivity $ map rewriteBinding bindings
  where
    rewriteBinding binding@(NamedVar path value position) =
        case staticKey path >>= (`KM.lookup` shapes) of
            Just shape -> NamedVar path (rewriteByShape shape value) position
            Nothing -> binding
    rewriteBinding binding = binding
rewriteRecord _ expression = expression

staticKey :: NonEmpty (NKeyName expression) -> Maybe Key.Key
staticKey (StaticKey (VarName name) :| []) = Just $ Key.fromText name
staticKey _ = Nothing

rewriteString :: NExpr -> NExpr
rewriteString (Fix (NStr (DoubleQuoted [Plain text])))
    | T.any (== '\n') text = mkIndentedStr 0 text
rewriteString expression = expression

renderStepExpr :: NExpr -> T.Text
renderStepExpr = renderStrict . layoutPretty defaultLayoutOptions . getDoc . foldFix renderNode
  where
    renderNode (NStr (Indented _ [Plain text])) =
        simpleExpr $ "''" <> hardline <> pretty (escapeIndented text) <> "''"
    renderNode node = exprFNixDoc node

escapeIndented :: T.Text -> T.Text
escapeIndented = preserveCommonIndent . T.replace "${" "''${" . T.replace "''" "'''"

preserveCommonIndent :: T.Text -> T.Text
preserveCommonIndent text
    | not (null contentLines) && all (T.isPrefixOf " ") contentLines = "${\"\"}" <> text
    | otherwise = text
  where
    contentLines = filter (not . T.null) $ T.splitOn "\n" text
