{-# LANGUAGE OverloadedStrings #-}
module Main where
import System.IO
import System.Environment
import System.FilePath.Posix
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import Data.Aeson
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Parser.Mod
import Codegen.File

-- Calls codegen and prints errors
cppWritter :: String -> Either String ByteString -> IO ()
cppWritter fn (Right cpp) = B.writeFile fn cpp
cppWritter _ (Left s) = hPutStrLn stderr s

-- Parse JSON file into a Module
parse :: ByteString -> Either String Module
parse buffer = eitherDecode buffer :: Either String Module

dbgModule :: Module -> Either String ByteString
dbgModule mod = Left $ show mod

transModule :: Module -> Either String ByteString
transModule mod = Right $ (B.pack . T.unpack . renderStrict . layoutPretty layoutOptions . pretty . toCFile) mod
    where layoutOptions = LayoutOptions { layoutPageWidth = AvailablePerLine 180 1 }


main :: IO ()
main = do
  argv <- getArgs
  mapM_ (\arg -> do
    json <- B.readFile arg
    let newfilename = addExtension ((dropExtension . takeFileName) arg) "cpp"
        cpp = parse json >>= transModule
    cppWritter newfilename cpp) argv

