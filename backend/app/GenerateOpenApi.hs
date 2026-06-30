{- | Writes the backend OpenAPI specification to a JSON file.

Usage: @generate-openapi [OUTPUT_PATH]@ (defaults to @docs/pages/openapi.json@).
-}
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
            [] -> "docs/pages/openapi.json"
    LBS.writeFile out (encodePretty pointyOpenApi)
    putStrLn ("Wrote OpenAPI specification to " <> out)
