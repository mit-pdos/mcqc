{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Classparser.Parser where
import Classparser.Lexer
import CIR.Expr
import Types.Inference
import Data.Text (Text)
import Data.List.Split
import System.FilePath
import System.Directory
import Control.Monad
import Data.MonoTraversable
import qualified Data.Text         as T
import qualified Data.List         as L
import qualified Data.Map.Strict   as M
import qualified Data.Maybe        as MA

-- Given a path, look for all .v files and create the type Context
loadCtx :: FilePath -> IO (Context CType)
loadCtx classdir = do
    files <- listDirectory classdir
    let classes = filter (\fn -> ".v" == takeExtension fn) . map (classdir </>) $ files
    boundctxs <- forM classes (\fn -> do
            txt <- readFile fn
            let abstors = getAbstractors txt
            let (freets, boundts) = getPlugs txt
            -- Bind plugs to free variables
            let plugs = map (mkCType freets) boundts
            -- Replace Text with CType and plug instance types in one step
            return $ M.map (map (plug plugs . mkCType abstors)) $ getCtors txt)
    return (foldr mergeCtx M.empty boundctxs)

plug :: [CType] -> CType -> CType
plug binders CTFree { .. }
    -- Plug the binder in a free type, remember free types are 1-indexed
    | _idx <= length binders = binders !! (_idx - 1)
    -- Otherwise reduce free index
    | otherwise = CTFree $ _idx - length binders + 1
plug binders other = omap (plug binders) other

-- Make a CType from a Coq lexical type and abstractors
mkCType :: [Text] -> Text -> CType
mkCType abstractors tn
    -- If type name is in list of abstractors, it is free
    | tn `elem` abstractors = CTFree . (+1) .  MA.fromJust . L.elemIndex tn $ abstractors
    | "(" `T.isPrefixOf` tn &&
      ")" `T.isSuffixOf` tn = mkCType abstractors . T.drop 1 . T.dropEnd 1 $ tn
    -- Otherwise type is either CTBase
    | length terms == 1 = CTBase $ head terms
    -- Or a composite type, recurse in subtypes
    | length terms == 0 = error $ "Cannot parse type " ++ show tn
    | otherwise = CTExpr (head terms) (subterms terms)
    where terms = map T.pack . splitOn " " . T.unpack $ tn
          subterms = map (mkCType abstractors) . tail


