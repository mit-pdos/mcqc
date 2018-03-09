{-# LANGUAGE DuplicateRecordFields, OverloadedStrings, TemplateHaskell #-}
module Main where
import Prelude hiding (readFile, writeFile)
import System.IO hiding (readFile, writeFile)
import System.Environment
import System.IO (hPutStrLn)
import Data.ByteString.Lazy.Char8 (ByteString, unpack, pack, writeFile, concat, readFile)
import Codegen
import Boogie.AST

-- Calls codegen and prints errors
boogieWriter :: String -> Either String ByteString -> IO ()
boogieWriter fn (Right bpl) = writeFile fn bpl
boogieWriter _ (Left s) = hPutStrLn stderr s

main :: IO ()
main = do
  argv <- getArgs
  mapM_ (\arg -> do
    coq_json <- readFile arg
    let boogie_out= (parse coq_json >>= makeModule)
    boogieWriter "foo.bpl" boogie_out) argv

