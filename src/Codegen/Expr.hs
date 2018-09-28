{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Codegen.Expr where
import CIR.Expr
import Codegen.Rewrite
import Common.Flatten
import Common.Inference
import Parser.Pattern
import Control.Monad.State
import Parser.Expr
import Debug.Trace

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

-- Type compiling, from Coq to C++
toCType :: Typ -> CType
toCType TypGlob    { targs = [], .. } = CTBase $ toCTBase name
toCType TypGlob    { .. }             = CTExpr (CTBase $ toCTBase name) (map toCType targs)
toCType TypVar     { .. }             = CTVar name $ map toCExpr args
toCType TypVaridx  { .. }             = CTFree idx
toCType TypDummy   {}                 = CTBase "void"
toCType TypUnknown {}                 = CTAuto
toCType t          {- TypArrow -}     = CTFunc (last typelist) (init typelist)
    where flattenType TypArrow { .. } = toCType left:flattenType right
          flattenType t               = [toCType t]
          nfreevars                   = foldl max 0 [getMaxVaridx i | i <- flattenType t]
          typelist                    = evalState (mapM raiseCTFunc $ flattenType t) nfreevars
          -- raise CTFuncs to template functions
          raiseCTFunc CTFunc { .. }   = do { m <- get; put (m+1); return $ CTFree (m+1) }
          raiseCTFunc CTExpr { .. }   = do { c <- raiseCTFunc _tbase; cargs <- mapM raiseCTFunc _tins; return $ CTExpr c cargs }
          raiseCTFunc o               = return o
