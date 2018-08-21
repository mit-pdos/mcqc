{-# LANGUAGE OverloadedStrings #-}
module Main where
import System.IO
import System.Environment
import System.FilePath.Posix
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Parser.Mod
import Codegen.File
import PrettyPrinter.File()
import Ops.Flags

-- Calls codegen and prints errors
cppWritter :: String -> Either String ByteString -> IO ()
cppWritter fn (Right cpp) = B.writeFile fn cpp
cppWritter _ (Left s) = hPutStrLn stderr s

-- TODO: Use namespaces to verify link of C++17 functions in place of their coq counterparts
transpile :: Module -> Either String ByteString
transpile mod = Right $ B.pack . T.unpack . renderStrict . layoutPretty layoutOptions . pretty . compile $ mod
    where layoutOptions = LayoutOptions { layoutPageWidth = AvailablePerLine 40 1 }

debug :: Module -> Either String ByteString
debug mod = Left $ B.unpack . encodePretty . compile $ mod

main :: IO ()
main = do
  (as, fs) <- getArgs >>= getFlags
  let pipeline = if Debug `elem` as then debug
                 else transpile
  let givefn = case getOutput as of
                (Nothing) -> (\arg -> 
                    addExtension ((dropExtension . takeFileName) arg) "cpp")
                (Just fn) -> (\_ -> fn)
  mapM_ (\arg -> B.readFile arg >>=
    (\json -> cppWritter (givefn arg) (eitherDecode json >>=
      pipeline))) fs
