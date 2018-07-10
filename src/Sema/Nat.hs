{-# LANGUAGE RecordWildCards, OverloadedStrings  #-}
module Sema.Nat where
import Data.Text (Text)
import Codegen.Expr
import Codegen.Utils

natSemantics :: CExpr -> CExpr
-- Semantics for O and S
natSemantics CExprCall { cfunc = "Datatypes.O", cparams = [] } = CExprNat 0
natSemantics CExprCall { cfunc = "Datatypes.O", cparams = [args] } = error "Datatypes.0 with args found!"
natSemantics CExprCall { cfunc = "Datatypes.S", cparams = [a] } = CExprNat $ (nat . natSemantics $ a) + 1
natSemantics CExprCall { cfunc = "Datatypes.S", cparams = a:arg } = error "Datatypes.S with more than one args found!"
-- Semantics for higher order expr
natSemantics CExprLambda { .. } = CExprLambda largs (natSemantics lbody)
natSemantics CExprCase   { .. } = CExprCase (natSemantics cexpr) (map natSemantics cases)
natSemantics CExprMatch  { .. } = CExprMatch mpat (natSemantics mbody)      -- TODO: Unify CPattern and CExpr
natSemantics CExprCtor   { .. } = CExprCtor cname cargs                     -- TODO: Why both this and CExprCall?
-- Reduced forms preserved
natSemantics other = other

pipeline = natSemantics
