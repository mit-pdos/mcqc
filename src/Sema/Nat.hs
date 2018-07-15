{-# LANGUAGE RecordWildCards, OverloadedStrings  #-}
module Sema.Nat where
import Data.Text (Text)
import Codegen.Expr
import Codegen.Utils

natSemantics :: CExpr -> CExpr
-- Semantics for O and S
natSemantics CExprCall { _fname = "Datatypes.O", _fparams = [] } = CExprNat 0
natSemantics CExprCall { _fname = "Datatypes.O", _fparams = [args] } = error "Datatypes.0 with args found!"
natSemantics CExprCall { _fname = "Datatypes.S", _fparams = [a] } = CExprNat $ (_nat . natSemantics $ a) + 1
natSemantics CExprCall { _fname = "Datatypes.S", _fparams = a:arg } = error "Datatypes.S with more than one args found!"
-- Semantics for higher order expr
natSemantics CExprLambda { .. } = CExprLambda _largs (natSemantics _lbody)
natSemantics CExprCase   { .. } = CExprCase (natSemantics _cexpr) (map natSemantics _cases)
natSemantics CExprMatch  { .. } = CExprMatch (natSemantics _mpat) (natSemantics _mbody)      -- TODO: Unify CPattern and CExpr
natSemantics CExprCtor   { .. } = CExprCtor _cname _cargs                     -- TODO: Why both this and CExprCall?
-- Reduced forms preserved
natSemantics other = other

