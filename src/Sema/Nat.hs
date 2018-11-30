{-# LANGUAGE RecordWildCards, OverloadedStrings  #-}
module Sema.Nat where
import CIR.Expr
import Data.MonoTraversable

-- Natural semantics
natSemantics :: CExpr -> CExpr
-- Semantics for O and S
natSemantics CExprCall { _fname = "O", _fparams = [] }     = CExprNat 0
natSemantics CExprCall { _fname = "S", _fparams = [a] } =
    let n = natSemantics a in
      case n of (CExprNat { .. }) -> CExprNat $ _nat + 1
                (_) -> CExprCall "succ" [n]
natSemantics CExprCall { _fname = "O", _fparams = [_] } = error "Datatypes.O with args found!"
natSemantics CExprCall { _fname = "S", _fparams = _:_ } = error "Datatypes.S with more than one args found!"
-- Propagate to children expr
natSemantics other                                                   = omap natSemantics other

