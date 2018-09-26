{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Common.Inference where
import CIR.Expr
import CIR.Decl
import Codegen.Rewrite
import Control.Monad.State
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Common.Config as Conf
import Control.Lens
import Debug.Trace

-- Traverse Type-tree for all typenames
getTypesT :: CType -> [Text]
-- getTypesT d | trace ("DBG type " ++ show d) False = undefined
getTypesT CTFunc { .. } = getTypesT _fret ++ concatMap getTypesT _fins
getTypesT CTExpr { .. } = getTypesT _tbase ++ concatMap getTypesT _tins
getTypesT CTVar  { .. } = concatMap getTypes _vargs
getTypesT CTBase { .. } = [T.toLower _base]
getTypesT _             = []

-- raise CTFuncs to template functions
raiseCTFunc :: CType -> State Int CType
raiseCTFunc CTFunc { .. } = do { m <- get; put (m+1); return $ CTFree (m+1) }
raiseCTFunc CTExpr { .. } = do { c <- raiseCTFunc _tbase; cargs <- mapM raiseCTFunc _tins; return $ CTExpr c cargs }
raiseCTFunc o             = return o

-- Get number of free parameters (Varidx)
getMaxVaridx :: CType -> Int
getMaxVaridx t = foldl max 0 $ getVaridxs t
    where getVaridxs CTFree { .. } = [_idx]
          getVaridxs CTFunc { .. } = getVaridxs _fret ++ concatMap getVaridxs _fins
          getVaridxs CTExpr { .. } = getVaridxs _tbase ++ concatMap getVaridxs _tins
          getVaridxs other         = []

-- Traverse type, look for all C++ template free variables and return them
-- ie: (List<T>,Optional<Q>) -> [T, Q]
getTemplates :: CType -> String
getTemplates t = take (getMaxVaridx t) ['T'..'Z']

-- Traverse AST for all typenames
getTypes :: CExpr -> [Text]
getTypes CExprSeq   { .. } = "proc":(getTypes _left ++ getTypes _right)
getTypes CExprCall  { _fname  = "some", .. } = "optional" : concatMap getTypes _fparams
getTypes CExprCall  { _fname  = "none", .. } = "optional" : concatMap getTypes _fparams
getTypes CExprCall  { _fname  = "show", .. } = "show" : concatMap getTypes _fparams
getTypes CExprCall  { .. } = _fname : concatMap getTypes _fparams
getTypes CExprStr   { .. } = ["String"]
getTypes CExprNat   { .. } = ["nat"]
getTypes CExprTuple { .. } = "tuple" : concatMap getTypes _items
getTypes CExprStmt  { .. } = "proc" : getTypesT _stype ++ getTypes _sbody
getTypes CExprList   { .. } = "list" : concatMap getTypes _elems
getTypes CExprLambda { .. } = _largs ++ getTypes _lbody
getTypes CExprBool   { .. } = ["bool"]
getTypes CExprVar    { .. } = []

