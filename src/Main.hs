module Main where
import Prelude hiding (readFile)
import Control.Applicative
import Data.Aeson
import Data.Text (Text)
import System.Environment
import Schema
import Data.ByteString.Lazy (readFile)

main = do
    args <- getArgs
    mapM_ (decode . readFile) args
