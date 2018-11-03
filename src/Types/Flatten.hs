{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Types.Flatten where
import CIR.Expr
import CIR.Decl
import Data.Text (Text)
import qualified Data.Text as T
import qualified Common.Config as Conf

-- This class is for instances with types, which can be flattened and returned by type-name
class Typeful a where
    gettypes :: a -> [Text]

instance Typeful CDef where
    gettypes CDef { .. } = gettypes _ty

instance Typeful CExpr where
    gettypes CExprSeq    { .. } = "proc":(gettypes _left ++ gettypes _right)
    gettypes CExprOption { _val = Just a }  = "option" : gettypes a
    gettypes CExprOption { _val = Nothing } = ["option"]
    gettypes CExprCall   { _fname  = "show", .. } = "show" : concatMap gettypes _fparams
    gettypes CExprCall   { .. } = _fname : concatMap gettypes _fparams
    gettypes CExprStr    { .. } = ["String"]
    gettypes CExprNat    { .. } = ["nat"]
    gettypes CExprTuple  { .. } = "tuple" : concatMap gettypes _items
    gettypes CExprStmt   { .. } = "proc" : gettypes _stype ++ gettypes _sbody
    gettypes CExprList   { .. } = "list" : concatMap gettypes _elems
    gettypes CExprLambda { .. } = _largs ++ gettypes _lbody
    gettypes CExprBool   { .. } = ["bool"]
    gettypes CExprVar    { .. } = []

instance Typeful CType where
    gettypes CTFunc { .. } = gettypes _fret ++ concatMap gettypes _fins
    gettypes CTExpr { .. } = T.toLower _tbase : concatMap gettypes _tins
    gettypes CTVar  { .. } = concatMap gettypes _vargs
    gettypes CTBase { .. } = [T.toLower _base]
    gettypes _             = []

instance Typeful CDecl where
    gettypes CDEmpty  {}     = []
    gettypes CDType   { _td = CDef { .. } } = gettypes _ty
    gettypes CDInd    { _id = CDef { .. }, .. } = gettypes _ty ++ concatMap (gettypes . snd) _ictors
    gettypes CDFunc   { _fd = CDef { .. }, .. } = gettypes _ty ++ concatMap gettypes _fargs ++ gettypes _fbody
    gettypes CDStruct { .. } = concatMap gettypes _fields
    gettypes CDExpr   { .. } = gettypes _expr

getIncludes :: CDecl -> [Text]
getIncludes = filter (`elem` Conf.libs) . gettypes

