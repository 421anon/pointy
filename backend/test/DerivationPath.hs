module Main (main) where

import BuildRunner (isDerivationPath)
import Control.Monad (unless)

main :: IO ()
main = do
    assertTrue "a store derivation" "/nix/store/00000000000000000000000000000000-pointy-test.drv"
    assertTrue "a derivation with a dashed name" "/nix/store/0123456789abcdfghijklmnpqrsvwxyz-name-with-dashes.drv"
    assertFalse "an option" "--option"
    assertFalse "an option with a drv suffix" "--option=/nix/store/00000000000000000000000000000000-pointy-test.drv"
    assertFalse "a relative path" "00000000000000000000000000000000-pointy-test.drv"
    assertFalse "a short hash" "/nix/store/0000-pointy-test.drv"
    assertFalse "a non derivation" "/nix/store/00000000000000000000000000000000-pointy-test"
    assertFalse "a nested path" "/nix/store/00000000000000000000000000000000-pointy-test.drv/output"
    assertFalse "a slash in the derivation name" "/nix/store/00000000000000000000000000000000-pointy/test.drv"
    assertFalse "a space in the derivation name" "/nix/store/00000000000000000000000000000000-pointy test.drv"
    assertFalse "an empty derivation name" "/nix/store/00000000000000000000000000000000-.drv"

assertTrue :: String -> String -> IO ()
assertTrue label path = unless (isDerivationPath path) $ fail (label ++ ": expected " ++ show path ++ " to be a derivation path")

assertFalse :: String -> String -> IO ()
assertFalse label path = unless (not (isDerivationPath path)) $ fail (label ++ ": expected " ++ show path ++ " not to be a derivation path")
