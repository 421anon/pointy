{-# LANGUAGE OverloadedStrings #-}

module NixUtils (sortAttrSet) where

import Data.Fix (Fix (..))
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text as T
import Nix.Expr.Types (Binding (..), NExpr, NExprF (..), NKeyName (..), VarName (..))

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
