{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Classparser.Parser where
import Classparser.Lexer
import CIR.Expr
import Types.Inference
import Data.Text (Text)
import Data.List.Split
import Data.Map.Strict (Map)
import System.FilePath
import System.Directory
import Control.Monad
import qualified Data.Text         as T
import qualified Data.List         as L
import qualified Data.Map.Strict   as M
import qualified Data.Maybe        as MA

-- Given a path, look for all .v files and create the type Context
loadCtx :: FilePath -> IO (Context CType)
loadCtx classdir = do
    files <- listDirectory classdir
    let vfns = filter (\fn -> ".v" == takeExtension fn) . map (classdir </>) $ files
    forM_ vfns (\fn -> do
        txt <- readFile fn
        let classes = getAbstractors txt
        let instances = getPlugs txt
        let ctors = getCtors txt
        putStrLn . show $ classes
        putStrLn . show $ instances
        putStrLn . show $ ctors
        return ())
    return $ M.fromList []


-- Make a CType from a Coq lexical type and abstractors
mkCType :: [Text] -> Text -> CType
mkCType abstractors tn
    -- If type name is in list of abstractors, it is free
    | tn `elem` abstractors = CTFree . MA.fromJust . L.elemIndex tn $ abstractors
    -- Otherwise type is either CTBase
    | length terms == 1 = base terms
    -- Or a composite type, recurse in subtypes
    | length terms == 0 = error $ "Cannot parse type " ++ show tn
    | otherwise = CTExpr (base terms) (subterms terms)
    where terms = map T.pack . splitOn " " . T.unpack $ tn
          base  = CTBase . head
          subterms = map (mkCType abstractors) . tail



