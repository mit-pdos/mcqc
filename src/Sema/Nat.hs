{-# LANGUAGE RecordWildCards, OverloadedStrings  #-}
module Sema.Nat where
import Sema.Common
import Codegen.Expr

-- Natural semantics
natSemantics :: CExpr -> CExpr
-- Semantics for O and S
natSemantics CExprCall { _fname = "Datatypes.O", _fparams = [] }     = CExprNat 0
natSemantics CExprCall { _fname = "Datatypes.O", _fparams = [args] } = error "Datatypes.0 with args found!"
natSemantics CExprCall { _fname = "Datatypes.S", _fparams = [a] }    = CExprNat $ (_nat . natSemantics $ a) + 1
natSemantics CExprCall { _fname = "Datatypes.S", _fparams = a:arg }  = error "Datatypes.S with more than one args found!"
natSemantics CExprCall { .. }                                        = CExprCall _fname (map natSemantics _fparams)
-- Propagate to children expr
natSemantics other                                                   = descend natSemantics other
