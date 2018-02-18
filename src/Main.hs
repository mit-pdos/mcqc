module Main where
import Control.Applicative
import Data.Aeson
import Data.Text (Text)
import System.Environment
import Schema

main :: IO ()

handleJsonFilename name = decode (readFile name) :: Maybe Value

main = do
    args <- getArgs
    mapM_ handleJsonFilename args
