{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.Except (runExceptT)
import Data.Text (Text)
import Effects (AppM)
import Handlers.Store (downloadHandler, listHandler)
import Interpreters.Production (runProduction)
import Servant (ServerError (..))

main :: IO ()
main = do
    expectStatus "parent of the output" 400 (listHandler output (Just ".."))
    expectStatus "parent reached through a subdirectory" 400 (listHandler output (Just "sub/../.."))
    expectStatus "parent segment that stays inside" 400 (listHandler output (Just "sub/../other"))
    expectStatus "sibling store path sharing the output prefix" 400 (downloadHandler output "/nix/store/00000000000000000000000000000000-out-sibling/data.csv")
    expectStatus "missing file inside the output" 404 (downloadHandler output "sub/data.csv")

output :: Text
output = "/nix/store/00000000000000000000000000000000-out"

expectStatus :: String -> Int -> AppM a -> IO ()
expectStatus label expected handler = do
    result <- runProduction (runExceptT handler)
    let actual = either errHTTPCode (const 200) result
    if actual == expected
        then pure ()
        else fail $ label ++ ": expected " ++ show expected ++ ", got " ++ show actual
