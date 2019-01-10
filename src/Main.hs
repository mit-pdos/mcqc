{-# LANGUAGE OverloadedStrings #-}
module Main where
import System.Environment
import Data.ByteString.Lazy.Char8 (ByteString)
import Control.Monad
import Control.Monad.State
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Codegen.Compiler
import Ops.Flags
import Types.Context
import Common.Filter
import CIR.File
import Parser.Mod
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import qualified Common.Config as Conf

-- Render from Pretty printer Doc to bytestring
render :: Doc ann -> ByteString
render = B.pack . T.unpack . renderStrict . layoutPretty layoutOptions
    where layoutOptions = LayoutOptions { layoutPageWidth = AvailablePerLine 40 1 }

main :: IO ()
main = do
    args <- getArgs
    (flags, fn) <- getFlags args
    -- Read AST
    ast <- readAst fn
    -- Handle flags
    forM_ flags (\flag ->
        case flag of
            (Output outfn) -> do
                let modules = filterMod . used_modules $ ast
                let extfn = map (`T.append` ".json") modules
                externals <- mapM (readAst . T.unpack) extfn
                -- All the compiling in one line
                let bigfile = flip evalState Conf.nativeContext (mconcat <$> mapM compile (externals ++ [ast]))
                B.writeFile outfn . render . pretty $ (bigfile :: CFile)
            (Debug) -> do
                putStrLn . header $ "Args"
                putStrLn . show $ args
                putStrLn . header $ "Modules Imported"
                let modules = filterMod . used_modules $ ast
                putStrLn . show $ modules
                putStrLn . header $ "JSON dump"
                let extfn = map (`T.append` ".json") modules
                externals <- mapM (readAst . T.unpack) extfn
                putStrLn . show $ externals
                -- All the compiling in one line
                let (bigfile, st) = flip runState Conf.nativeContext (mconcat <$> mapM compile (externals ++ [ast]))
                putStrLn . B.unpack . encodePretty $ (bigfile :: CFile)
                putStrLn . header $ "Context after compiling"
                printCtx st
            -- Default
            (o) -> error $ "Unhandled flag " ++ show o)
    where header s = (replicate 12 '=') ++ concat [" ", s, " "] ++ (replicate 12 '=')
          readAst fn = B.readFile fn >>=
                        (\json -> case eitherDecode json of
                            (Right r) -> return r
                            (Left s) -> error s)
