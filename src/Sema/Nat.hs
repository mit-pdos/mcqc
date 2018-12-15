{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Sema.Nat where
import CIR.Expr
import Data.MonoTraversable

-- Natural semantics
natSemantics :: CExpr -> CExpr
-- Semantics for O and S
natSemantics CExprCall { _cd = CDef { _nm = "O" }, _cparams = [] }  = CExprNat 0
natSemantics CExprCall { _cd = CDef { _nm = "S" }, _cparams = [a] } =
    case num of
      (CExprNat { .. }) -> CExprNat $ _nat + 1
      (_)               -> CExprCall cd [num]
    where num = natSemantics a
          cd  = CDef "succ" (CTBase "nat")
natSemantics CExprCall { _cd = CDef { _nm = "O" }, _cparams = _:_ }   = error "Datatypes.O with args found!"
natSemantics CExprCall { _cd = CDef { _nm = "S" }, _cparams = _:_:_ } = error "Datatypes.S with more than one args found!"
-- Propagate to children expr
natSemantics other = omap natSemantics other

