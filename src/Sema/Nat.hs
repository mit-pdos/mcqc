{-# LANGUAGE RecordWildCards, OverloadedStrings  #-}
module Sema.Nat where
import Data.Text (Text)
import Codegen.Expr
import Codegen.Utils
import Control.Lens

natSemantics :: CExpr -> CExpr
-- Semantics for O and S
natSemantics CExprCall { _fname = "Datatypes.O", _fparams = [] } = CExprNat 0
natSemantics CExprCall { _fname = "Datatypes.O", _fparams = [args] } = error "Datatypes.0 with args found!"
natSemantics CExprCall { _fname = "Datatypes.S", _fparams = [a] } = CExprNat $ (_nat . natSemantics $ a) + 1
natSemantics CExprCall { _fname = "Datatypes.S", _fparams = a:arg } = error "Datatypes.S with more than one args found!"
-- Propagate to children expr
natSemantics CExprLambda { .. } = CExprLambda _largs (natSemantics _lbody)
natSemantics CExprCase   { .. } = CExprCase (natSemantics _cexpr) (map natSemantics _cases)
natSemantics CExprMatch  { .. } = CExprMatch (natSemantics _mpat) (natSemantics _mbody)
natSemantics CExprCall   { .. } = CExprCall _fname (map natSemantics _fparams)
natSemantics CExprTuple  { .. } = CExprTuple (map natSemantics _items)
-- Transparent to the rest of expressions
natSemantics other = other

