module Main where
import Prelude hiding (readFile, writeFile)
import System.IO hiding (readFile, writeFile)
import System.Environment
import System.IO (hPutStrLn)
import Data.ByteString.Lazy.Char8 (ByteString, unpack, pack, writeFile, concat, readFile)
import Codegen

-- Calls codegen and prints errors
boogieWriter :: String -> Either String ByteString -> IO ()
boogieWriter fn (Right bpl) = writeFile fn bpl
boogieWriter _ (Left s) = hPutStrLn stderr s

main :: IO ()
main = do
  argv <- getArgs
  mapM_ (\arg -> do
    coq_json <- readFile arg
    let boogie_out= (makeModule coq_json >>= codegen)
    boogieWriter "foo.bpl" boogie_out) argv

