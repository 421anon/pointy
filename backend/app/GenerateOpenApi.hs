module Main (main) where

import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as LBS
import Docs.OpenApi (pointyOpenApi)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    let out = case args of
            (path : _) -> path
            [] -> "openapi.json"
    LBS.writeFile out (encodePretty pointyOpenApi)
    putStrLn ("Wrote OpenAPI specification to " <> out)
