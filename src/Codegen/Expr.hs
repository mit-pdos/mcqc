{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
module Codegen.Expr where
import CIR.Expr
import Parser.Expr
import Parser.Pattern
import Codegen.Rewrite
import Common.Utils
import Control.Monad.State
import Data.Text (Text)
import qualified Data.List  as L
import qualified Data.Maybe as MA

-- Expression compiling, from Coq to C++
toCExpr :: Expr -> CExpr
-- toCExpr d | trace ("=== DBG Expr.hs/toCExpr " ++ show d) False = undefined
toCExpr ExprLambda      { .. } = CExprLambda defs $ toCExpr body
    where defs = map mkdef argnames
toCExpr ExprCase        { .. } = CExprCall (mkdef "match") $ toCExpr expr:map mkLambda cases
    where mkLambda Case    { .. } = CExprLambda (mkdefs pat) $ toCExpr body
          mkdefs                  = map mkdef . getArgs
          getArgs PatCtor  { .. } = argnames
          getArgs PatTuple { .. } = concatMap getArgs items
          getArgs PatRel   { .. } = [name]
          getArgs PatWild  {}     = ["_"]
toCExpr ExprConstructor { .. } = CExprCall (mkdef name) $ map toCExpr args
toCExpr ExprApply       { func = ExprGlobal { .. }, .. } = CExprCall (mkdef name) $ map toCExpr args
toCExpr ExprApply       { func = ExprRel    { .. }, .. } = CExprCall (mkdef name) $ map toCExpr args
toCExpr ExprApply       { func = ExprLambda { argnames = h:_, .. }, .. } = CExprCall (mkdef h) $ map toCExpr args
toCExpr ExprApply       { func = ExprCoerce { .. }, .. } = toCExpr $ ExprApply value args
--    where assignment = CExprStmt (mkdef name) $ toCExpr nameval
toCExpr ExprLet         { .. } = assignment <> (toCExpr body)
    where assignment = CExprStmt (mkdef name) $ toCExpr nameval
toCExpr ExprRel         { .. } = CExprVar . toCName $ name
toCExpr ExprGlobal      { .. } = CExprVar . toCName $ name
toCExpr ExprCoerce      { .. } = toCExpr value
toCExpr ExprDummy       {}     = CExprVar ""

-- Transcribe to CType with a list of abstractors
toCTypeAbs :: [Text] -> Typ -> CType
toCTypeAbs abs TypVar  { .. }
    | name `elem` abs = CTFree . (+1) . MA.fromJust . L.elemIndex name $ abs
    | otherwise = CTVar (toCTBase name) $ map toCExpr args
toCTypeAbs _ TypGlob    { targs = [], .. } = CTBase $ toCTBase name
toCTypeAbs abs TypGlob         { .. } = CTExpr (toCTBase name) $ map (toCTypeAbs abs) targs
toCTypeAbs abs TypVaridx       { .. } = CTFree $ idx + length abs
toCTypeAbs _ TypDummy          {}     = CTBase "void"
toCTypeAbs _ TypUnknown        {}     = CTAuto
toCTypeAbs abs t@TypArrow { .. }      = CTFunc (last typelist) (init typelist)
    where flattenType TypArrow { .. } = toCTypeAbs abs left:flattenType right
          flattenType t               = [toCTypeAbs abs t]
          nfreevars                   = foldl max 0 [getMaxVaridx i | i <- flattenType t]
          typelist                    = evalState (mapM raiseCTFunc $ flattenType t) nfreevars
          -- raise CTFuncs to templates
          raiseCTFunc CTFunc { .. }   = do { m <- get; put (m+1); return $ CTFree (m+1) }
          raiseCTFunc CTExpr { .. }   = CTExpr _tbase <$> mapM raiseCTFunc _tins
          raiseCTFunc o               = return o

-- Type compiling, from Coq to C++
toCType :: Typ -> CType
toCType = toCTypeAbs []

