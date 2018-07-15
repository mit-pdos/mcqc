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
import Clang.FuncSig
import Clang.CParser
import Clang.Linker
import PrettyPrinter.File
import System.Directory

-- Calls codegen and prints errors
cppWritter :: String -> Either String ByteString -> IO ()
cppWritter fn (Right cpp) = B.writeFile fn cpp
cppWritter _ (Left s) = hPutStrLn stderr s

-- Parse JSON file into a Module
parse :: ByteString -> Either String Module
parse buffer = eitherDecode buffer :: Either String Module

dbgModule :: Module -> Either String ByteString
dbgModule mod = Left $ show mod

-- TODO: Use nss to link C functions in place of their coq counterparts
transModule :: Module -> [ Namespace ] -> Either String ByteString
transModule mod nss = Right $ (B.pack . T.unpack . renderStrict . layoutPretty layoutOptions . pretty . (link nss) . compile) mod
    where layoutOptions = LayoutOptions { layoutPageWidth = AvailablePerLine 180 1 }

debug :: Module -> [ Namespace ] -> Either String ByteString
debug mod nss = Left $ (B.unpack . encodePretty . (link nss) . compile) mod

main :: IO ()
main = do
  argv <- getArgs
  files <- listDirectory "include"
  let clibs = filter isLib $ map (\s -> "include/" ++ s) files
  cnamespaces <- mapM (\file -> parseHpp file) clibs
  mapM_ (\arg -> do
    json <- B.readFile arg;
    let newfilename = addExtension ((dropExtension . takeFileName) arg) "cpp"
        cpp = parse json >>= (\m -> debug m cnamespaces)
    cppWritter newfilename cpp) argv

