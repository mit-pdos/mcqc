{-# LANGUAGE RecordWildCards, OverloadedStrings  #-}
module Sema.Nat where
import Data.Text (Text)
import Codegen.Expr
import Codegen.Utils
import Control.Lens

natSemanticsCall :: CExpr -> CExpr
-- Semantics for O and S
natSemanticsCall CExprCall { _fname = "Datatypes.O", _fparams = [] } = CExprNat 0
natSemanticsCall CExprCall { _fname = "Datatypes.O", _fparams = [args] } = error "Datatypes.0 with args found!"
natSemanticsCall CExprCall { _fname = "Datatypes.S", _fparams = [a] } = CExprNat $ (_nat . natSemanticsCall $ a) + 1
natSemanticsCall CExprCall { _fname = "Datatypes.S", _fparams = a:arg } = error "Datatypes.S with more than one args found!"
natSemanticsCall other = other

-- Semantics for higher order expr
natSemantics :: CExpr -> CExpr
natSemantics = over lbody natSemanticsCall
                  . over (items . traverse) natSemanticsCall
                  . over cexpr natSemanticsCall
                  . over (cases . traverse) natSemanticsCall
                  . over mpat natSemanticsCall
                  . over mbody natSemanticsCall
                  . over (fparams . traverse) natSemanticsCall
                  . over (items . traverse) natSemanticsCall
