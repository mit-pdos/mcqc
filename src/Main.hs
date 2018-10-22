{-# LANGUAGE OverloadedStrings #-}
module Main where
import System.IO
import System.Environment
import System.FilePath.Posix
import System.Directory
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import Control.Monad
import Data.Aeson
import Types.Inference
import Data.Aeson.Encode.Pretty
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Codegen.Compiler
import PrettyPrinter.File()
import Ops.Flags
import CIR.File
import Classparser.Parser

-- TODO: Use namespaces to verify link of C++17 functions in place of their coq counterparts
prettyprint  :: CFile -> ByteString
prettyprint = B.pack . T.unpack . renderStrict . layoutPretty layoutOptions . pretty
    where layoutOptions = LayoutOptions { layoutPageWidth = AvailablePerLine 40 1 }

main :: IO ()
main = do
    args <- getArgs
    (flags, fn) <- getFlags args
    -- Parse typeclasses
    libpath <- getLibDir flags
    -- Load context
    context <- loadCtx libpath
    -- Read AST
    jsonast <- B.readFile fn
    -- Check if JSON parsing was a success
    let ast = case eitherDecode jsonast of
                  (Right r) -> r
                  (Left s) -> error s
    -- Keep executable flags, the others are config
    let eflags = case filter isExec flags of
                     ([]) -> [Output fn]
                     (a)  -> a
    -- Handle flags
    forM_ eflags (\flag ->
        case flag of
            (Output outfn) -> do
                let cppast = prettyprint . compile context $ ast
                B.writeFile (outfn -<.> "cpp") cppast
            (Debug) -> do
                putStrLn $ header "Typeclass context"
                printCtx context
                putStrLn $ header "JSON dump"
                let cppast = B.unpack . encodePretty . compile context $ ast
                hPutStrLn stderr cppast
            -- Default
            (o) -> error $ "Unhandled flag " ++ show o)

    where getLibDir (Libs p:_) = return p
          getLibDir (_:ts)     = getLibDir ts -- If not found, assume its in `./classes`
          getLibDir ([])       = getCurrentDirectory >>= \d -> return (d </> "classes")
          isExec (Libs _) = False
          isExec (_)      = True
          header s = (replicate 12 '=') ++ concat [" ", s, " "] ++ (replicate 12 '=')
