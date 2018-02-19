module Main where
import Prelude hiding (readFile, writeFile, putStrLn)
import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Maybe
import Data.Text (Text)
import System.Environment
import Schema
import Utilities
import Data.ByteString.Lazy.Char8 (ByteString, writeFile, readFile, putStrLn)
import Data.Aeson.Encode.Pretty

-- Pure codegen function to be used as Functor
codegen :: Value -> ByteString
codegen _ = "TO BE FILED LATER"

errorHandler :: Maybe Value -> IO ()
errorHandler (Just ast) = do { writeFile "test.bpl" $ codegen ast; print ast}
errorHandler Nothing = putStrLn "Error parsing input file"

jsonReader :: String -> IO (Maybe Value)
jsonReader filename = parseModule <$> readFile filename

-- main :: IO Bool
main = do {
    [arg] <- getArgs;
    errorHandler =<< jsonReader arg;
    -- and <$> mapM check parseFile args;
}
