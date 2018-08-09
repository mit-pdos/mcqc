{-# LANGUAGE OverloadedStrings #-}
module Main where
import System.IO
import System.Environment
import System.FilePath.Posix
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Codegen.File
import Parser.Mod
import Clang.CParser
import PrettyPrinter.File
import System.Directory
import Ops.Flags

-- Calls codegen and prints errors
cppWritter :: String -> Either String ByteString -> IO ()
cppWritter fn (Right cpp) = B.writeFile fn cpp
cppWritter _ (Left s) = hPutStrLn stderr s

-- Parse JSON file into a Module
parse :: ByteString -> Either String Module
parse buffer = eitherDecode buffer :: Either String Module

-- TODO: Use namespaces to verify link of C++17 functions in place of their coq counterparts
transpile :: Module -> Either String ByteString
transpile mod = Right $ B.pack . T.unpack . renderStrict . layoutPretty layoutOptions . pretty . compile $ mod
    where layoutOptions = LayoutOptions { layoutPageWidth = AvailablePerLine 40 1 }

debug :: Module -> Either String ByteString
debug mod = Left $ B.unpack . encodePretty . compile $ mod

--getNamespaces :: IO [Namespaces]
--getNamespaces = do
--  files <- listDirectory "include"
--  let clibs = filter isLib $ map (\s -> "include/" ++ s) files
--  return $ mapM (\file -> parseHpp file) clibs

main :: IO ()
main = do
  (as, fs) <- getArgs >>= getFlags
  let pipeline = if Debug `elem` as then debug
                 else transpile
  let givefn = case getOutput as of
                (Nothing) -> (\arg -> addExtension ((dropExtension . takeFileName) arg) "cpp")
                (Just fn) -> (\_ -> fn)
  mapM_ (\arg -> B.readFile arg >>=
    (\json -> cppWritter (givefn arg) (parse json >>=
      pipeline))) fs
