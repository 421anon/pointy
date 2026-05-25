{-# LANGUAGE OverloadedStrings #-}

module NixUtils (isValidStorePath, sortAttrSet) where

import Data.Fix (Fix (..))
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text as T
import Nix.Expr.Types (Binding (..), NExpr, NExprF (..), NKeyName (..), VarName (..))
import ProcessLimiter (readProcessWithExitCodeL)
import System.Exit (ExitCode (..))

{- | True iff Nix considers the path a valid store path.

Do not stat the filesystem first: in dev-vm, Nix talks to the host
daemon while /nix/store is an overlay over a 9p-mounted host store.
A pre-build negative lookup for a future output can stay cached in the
overlay and hide the path even after the host daemon creates it.
-}
isValidStorePath :: FilePath -> IO Bool
isValidStorePath path = do
    (code, _, _) <- readProcessWithExitCodeL "nix" ["--offline", "path-info", path] ""
    return $ case code of
        ExitSuccess -> True
        ExitFailure _ -> False

sortAttrSet :: NExpr -> NExpr
sortAttrSet (Fix expr) = Fix $ case expr of
    NSet recursivity bindings -> NSet recursivity (sortBindings $ map sortBindingValue bindings)
    NList xs -> NList (map sortAttrSet xs)
    NLet bindings body -> NLet (sortBindings $ map sortBindingValue bindings) (sortAttrSet body)
    other -> other
  where
    sortBindingValue :: Binding NExpr -> Binding NExpr
    sortBindingValue (NamedVar path val pos) = NamedVar path (sortAttrSet val) pos
    sortBindingValue b = b

    sortBindings :: [Binding NExpr] -> [Binding NExpr]
    sortBindings = sortOn bindingKey

    bindingKey :: Binding NExpr -> T.Text
    bindingKey (NamedVar (StaticKey (VarName name) :| _) _ _) = name
    bindingKey (NamedVar (DynamicKey _ :| _) _ _) = ""
    bindingKey (Inherit{}) = ""
