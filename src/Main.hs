module Main where
import Data.Aeson
import Prelude hiding (readFile, writeFile)
import System.IO hiding (readFile, writeFile)
import System.Environment
import System.IO (hPutStrLn)
import Data.ByteString.Lazy.Char8 (ByteString, writeFile, readFile)
import Codegen.Schema (Module)
import Codegen.Mod (makeModule)

-- Calls codegen and prints errors
cppWritter :: String -> Either String ByteString -> IO ()
cppWritter fn (Right cpp) = writeFile fn cpp
cppWritter _ (Left s) = hPutStrLn stderr s

-- Parse JSON file into a Module
parse :: ByteString -> Either String Module
parse buffer = eitherDecode buffer :: Either String Module

main :: IO ()
main = do
  argv <- getArgs
  mapM_ (\arg -> do
    json <- readFile arg
    let cpp = (parse json >>= makeModule)
    cppWritter "foo.cpp" cpp) argv

