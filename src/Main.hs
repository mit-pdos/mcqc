module Main where
import Prelude hiding (readFile, writeFile, putStrLn)
import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Either
import Data.Text (Text)
import System.Environment
import Schema
import Utilities
import Data.ByteString.Lazy.Char8 (ByteString, writeFile, readFile, putStrLn)

-- Pure codegen function to be used as Functor
codegen :: Module -> ByteString
codegen _ = "TO BE FILED LATER"

errorHandler :: Either String Module -> IO ()
errorHandler (Right ast) = do { writeFile "test.bpl" $ codegen ast; print ast}
errorHandler (Left err) = putStr $ "Error parsing input file " ++ err ++ "\n"

jsonReader :: String -> IO (Either String Module)
jsonReader filename = parseModule <$> readFile filename

-- main :: IO Bool
main = do {
    [arg] <- getArgs;
    errorHandler =<< jsonReader arg;
    -- and <$> mapM check parseFile args;
}
