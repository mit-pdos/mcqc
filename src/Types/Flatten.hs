{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Types.Flatten where
import CIR.Expr
import CIR.Decl
import Codegen.Rewrite
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Common.Config as Conf
import Control.Lens
import Debug.Trace

-- Function type to list of args, with the return type in the end
getFuncT :: CType -> [CType]
getFuncT CTFunc  { .. } = _fins ++ [_fret]
getFuncT o = [o]

-- Traverse Type-tree for all typenames
getTypesT :: CType -> [Text]
-- getTypesT d | trace ("DBG type " ++ show d) False = undefined
getTypesT CTFunc { .. } = getTypesT _fret ++ concatMap getTypesT _fins
getTypesT CTExpr { .. } = T.toLower _tbase : concatMap getTypesT _tins
getTypesT CTVar  { .. } = concatMap getTypes _vargs
getTypesT CTBase { .. } = [T.toLower _base]
getTypesT _             = []

-- Get number of free parameters (Varidx)
getMaxVaridx :: CType -> Int
getMaxVaridx t = foldl max 0 $ getVaridxs t
    where getVaridxs CTFree { .. } = [_idx]
          getVaridxs CTFunc { .. } = getVaridxs _fret ++ concatMap getVaridxs _fins
          getVaridxs CTExpr { .. } = concatMap getVaridxs _tins
          getVaridxs other         = []

-- Traverse type, look for all C++ template free variables and return them
-- ie: (List<T>,Option<Q>) -> [T, Q]
getTemplates :: CType -> String
getTemplates t = take (getMaxVaridx t) ['T'..'Z']

-- Traverse AST for all typenames
getTypes :: CExpr -> [Text]
getTypes CExprSeq    { .. } = "proc":(getTypes _left ++ getTypes _right)
getTypes CExprOption { _val = Just a }  = "option" : getTypes a
getTypes CExprOption { _val = Nothing } = ["option"]
getTypes CExprCall   { _fname  = "show", .. } = "show" : concatMap getTypes _fparams
getTypes CExprCall   { .. } = _fname : concatMap getTypes _fparams
getTypes CExprStr    { .. } = ["String"]
getTypes CExprNat    { .. } = ["nat"]
getTypes CExprTuple  { .. } = "tuple" : concatMap getTypes _items
getTypes CExprStmt   { .. } = "proc" : getTypesT _stype ++ getTypes _sbody
getTypes CExprList   { .. } = "list" : concatMap getTypes _elems
getTypes CExprLambda { .. } = _largs ++ getTypes _lbody
getTypes CExprBool   { .. } = ["bool"]
getTypes CExprVar    { .. } = []

-- Traverse Declarations for libraries
getIncludes :: CDecl -> [Text]
getIncludes CDEmpty {}     = []
getIncludes CDType  { .. } = filter (`elem` Conf.libs) $ getTypesT _tval
getIncludes CDInd   { .. } = filter (`elem` Conf.libs) $ getTypesT _itype ++ ctorargs
    where ctorargs = concatMap (concatMap getTypesT . snd) $ _ictors
getIncludes CDFunc  { .. } = filter (`elem` Conf.libs) $ typargs ++ bodyargs
    where typargs = getTypesT _ftype
          bodyargs = getTypes _fbody
