{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
module Codegen.Expr where
import CIR.Expr
import Codegen.Rewrite
import Parser.Pattern
import Common.Utils
import Control.Monad.State
import Parser.Expr
import Data.Text (Text)
import qualified Data.List  as L
import qualified Data.Maybe as MA

-- Expression compiling, from Coq to C++
toCExpr :: Expr -> CExpr
-- toCExpr d | trace ("=== DBG Expr.hs/toCExpr " ++ show d) False = undefined
toCExpr ExprLambda      { .. } = CExprLambda argnames $ toCExpr body
toCExpr ExprCase        { .. } = CExprCall "match" $ toCExpr expr:map mkLambda cases
    where mkLambda Case    { .. } = CExprLambda (getArgs pat) (toCExpr body)
          getArgs PatCtor  { .. } = argnames
          getArgs PatTuple { .. } = concatMap getArgs items
          getArgs PatRel   { .. } = [name]
          getArgs PatWild  {}     = ["_"]
toCExpr ExprConstructor { .. } = CExprCall name $ map toCExpr args
toCExpr ExprApply       { func = ExprGlobal { .. }, .. } = CExprCall name $ map toCExpr args
toCExpr ExprApply       { func = ExprRel    { .. }, .. } = CExprCall name $ map toCExpr args
toCExpr ExprApply       { func = ExprLambda { .. }, .. } = CExprCall (head argnames) (map toCExpr args)
toCExpr ExprApply       { func = ExprCoerce { .. }, .. } = toCExpr $ ExprApply value args
toCExpr ExprLet         { .. } = CExprSeq assignment (toCExpr body)
    where assignment = CExprStmt CTAuto name $ toCExpr nameval
toCExpr ExprRel         { .. } = CExprVar name
toCExpr ExprGlobal      { .. } = CExprVar name
toCExpr ExprCoerce      { .. } = toCExpr value
toCExpr ExprDummy       {}     = CExprVar ""

-- Transcribe to CType with a list of abstractors
toCTypeAbs :: [Text] -> Typ -> CType
toCTypeAbs abs TypVar  { .. }
    | name `elem` abs = CTFree . MA.fromJust . L.elemIndex name $ abs
    | otherwise = CTVar (toCTBase name) $ map toCExpr args
toCTypeAbs _ TypGlob    { targs = [], .. } = CTBase $ toCTBase name
toCTypeAbs abs TypGlob    { .. }
    | name == "Datatypes.prod" = CTExpr (toCTBase name) $ concatMap prodlist targs
    | otherwise = CTExpr (toCTBase name) $ map (toCTypeAbs abs) targs
    where prodlist TypGlob { name = "Datatypes.prod", targs = [a,b] } = prodlist a ++ prodlist b
          prodlist o = [toCTypeAbs abs o]
toCTypeAbs abs TypVaridx       { .. } = CTFree $ idx + length abs
toCTypeAbs _ TypDummy          {}     = CTBase "void"
toCTypeAbs _ TypUnknown        {}     = CTAuto
toCTypeAbs abs t@TypArrow { .. }      = CTFunc (last typelist) (init typelist)
    where flattenType TypArrow { .. } = toCTypeAbs abs left:flattenType right
          flattenType t               = [toCTypeAbs abs t]
          nfreevars                   = foldl max 0 [getMaxVaridx i | i <- flattenType t]
          typelist                    = evalState (mapM raiseCTFunc $ flattenType t) nfreevars
          -- raise CTFuncs to template functions
          raiseCTFunc CTFunc { .. }   = do { m <- get; put (m+1); return $ CTFree (m+1) }
          raiseCTFunc CTExpr { .. }   = do { cargs <- mapM raiseCTFunc _tins; return $ CTExpr _tbase cargs }
          raiseCTFunc o               = return o

-- Type compiling, from Coq to C++
toCType :: Typ -> CType
toCType = toCTypeAbs []

