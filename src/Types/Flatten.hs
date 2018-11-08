{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Types.Flatten where
import CIR.Expr
import CIR.Decl
import Data.Text (Text)
import qualified Data.Text as T
import qualified Common.Config as Conf

-- This class is for instances with types, which can be flattened and returned by type-name
class Typeful a where
    getincludes  :: a -> [Text]

instance Typeful CDef where
    getincludes CDef { .. } = getincludes _ty

instance Typeful CExpr where
    getincludes CExprSeq    { .. } = "proc":(getincludes _left ++ getincludes _right)
    getincludes CExprOption { _val = Just a }  = "option" : getincludes a
    getincludes CExprOption { _val = Nothing } = ["option"]
    getincludes CExprCall   { _fname  = "show", .. } = "show" : concatMap getincludes _fparams
    getincludes CExprCall   { .. } = _fname : concatMap getincludes _fparams
    getincludes CExprStr    { .. } = ["String"]
    getincludes CExprNat    { .. } = ["nat"]
    getincludes CExprTuple  { .. } = "tuple" : concatMap getincludes _items
    getincludes CExprStmt   { .. } = "proc" : getincludes _sd ++ getincludes _sbody
    getincludes CExprList   { .. } = "list" : concatMap getincludes _elems
    getincludes CExprLambda { .. } = concatMap getincludes _lds ++ getincludes _lbody
    getincludes CExprBool   { .. } = ["bool"]
    getincludes CExprVar    { .. } = []

instance Typeful CType where
    getincludes CTFunc { .. } = getincludes _fret ++ concatMap getincludes _fins
    getincludes CTExpr { _tbase = "std::variant", .. } = "variant":concatMap getincludes _tins
    getincludes CTExpr { .. } = T.toLower _tbase : concatMap getincludes _tins
    getincludes CTVar  { .. } = concatMap getincludes _vargs
    getincludes CTBase { .. } = [T.toLower _base]
    getincludes _             = []

instance Typeful CDecl where
    getincludes CDEmpty  {}     = []
    getincludes CDType   { _td = CDef { .. } } = getincludes _ty
    getincludes CDInd    { _id = CDef { .. }, .. } = getincludes _ty ++ concatMap (getincludes . snd) _ictors
    getincludes CDFunc   { _fd = CDef { .. }, .. } = getincludes _ty ++ concatMap getincludes _fargs ++ getincludes _fbody
    getincludes CDStruct { .. } = concatMap getincludes _fields
    getincludes CDExpr   { .. } = getincludes _expr

getAllowedIncludes :: CDecl -> [Text]
getAllowedIncludes = filter (`elem` Conf.libs) . getincludes

