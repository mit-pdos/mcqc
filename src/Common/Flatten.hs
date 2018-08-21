{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Common.Flatten where
import Codegen.Expr
import Codegen.Decl
import Codegen.Rewrite
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Common.Config as Conf
import Control.Lens
import Debug.Trace

-- Traverse Type-tree for all typenames
getTypesT :: CType -> [Text]
-- getTypesT d | trace ("=== DEBUG " ++ show d) False = undefined
getTypesT CTFunc { .. } = getTypesT _fret ++ (concat $ map getTypesT _fins)
getTypesT CTExpr { .. } = getTypesT _tbase ++ (concat $ map getTypesT _tins)
getTypesT CTVar  { .. } = concat $ map getTypes _vargs
getTypesT CTBase { .. } = [T.toLower _base]
getTypesT _             = []

-- Traverse type, look for all C++ template free variables and return them
-- ie: (List<T>,Optional<Q>) -> [T, Q]
getTemplates :: CType -> [Char]
getTemplates CTFunc { .. } = getTemplates _fret ++ (concat $ map getTemplates _fins)
getTemplates CTExpr { .. } = getTemplates _tbase ++ (concat $ map getTemplates _tins)
getTemplates CTFree { .. } = [['T'..'Z'] !! (_idx - 1)]
getTemplates other         = []

-- Traverse AST for all typenames
getTypes :: CExpr -> [Text]
getTypes CExprSeq   { .. } = "proc":(getTypes _left ++ getTypes _right)
getTypes CExprCall  { _fname  = "some", .. } = "optional":(concat $ map getTypes _fparams)
getTypes CExprCall  { _fname  = "none", .. } = "optional":(concat $ map getTypes _fparams)
getTypes CExprCall  { .. } = _fname:(concat $ map getTypes _fparams)
getTypes CExprStr   { .. } = ["string"]
getTypes CExprNat   { .. } = ["nat"]
getTypes CExprTuple { .. } = "tuple":(concat $ map getTypes _items)
getTypes CExprStmt  { .. } = "proc":(getTypesT _stype) ++ getTypes _sbody
getTypes CExprList   { .. } = "list":(concat $ map getTypes _elems)
getTypes CExprLambda { .. } = _largs ++ (getTypes _lbody)
getTypes CExprBool   { .. } = []
getTypes CExprVar    { .. } = []

-- Traverse Declarations for libraries
getLibs :: CDecl -> [Text]
getLibs CDEmpty {}     = []
getLibs CDType  { .. } = nub . filter (flip elem Conf.libs) $ getTypesT _tval
getLibs CDFunc  { .. } = nub . filter (flip elem Conf.libs) $ typargs ++ bodyargs
    where typargs = getTypesT _ftype
          bodyargs = getTypes _fbody

-- Propagate to children expr
descend :: (CExpr -> CExpr) -> CExpr -> CExpr
descend f   CExprCall   { .. } = CExprCall _fname $ map f _fparams
descend f   CExprStmt   { .. } = CExprStmt _stype _sname $ f _sbody
descend f   CExprSeq    { .. } = CExprSeq (f _left) (f _right)
descend f   CExprTuple  { .. } = CExprTuple $ map f _items
descend f   CExprList   { .. } = CExprList $ map f _elems
descend f   CExprLambda { .. } = CExprLambda _largs $ f _lbody
-- If it doesn't match anything, then it's a normal form, ignore
descend _   other              = other

-- Apply toCTBase to all CType
retypes :: CType -> CType
retypes =
 -- single step lenses
 over fret retypes
 . over base toCTBase
 -- recursive lenses
 . over tbase retypes
 . over (tins . traverse) retypes
 . over (fins . traverse) retypes

-- Apply toCName to a CExpr
renames :: CExpr -> CExpr
renames =
 -- single step lenses
 over fname toCName
 . over str toCName
 . over var toCName
 -- nested definition lenses
 . over (largs . traverse) toCName
 -- recursive lenses
 . over lbody renames
 . over sbody renames
 . over left renames
 . over right renames
 . over (items . traverse) renames
 . over (fparams . traverse) renames
 . over (items . traverse) renames
 . over (elems . traverse) renames

