{-# LANGUAGE OverloadedStrings  #-}
module Sema.Bool where
import Data.MonoTraversable
import CIR.Expr

-- TODO: add more bool expression semantics
boolSemantics :: CExpr -> CExpr
-- Semantics for True and False
boolSemantics CExprCall { _cd = CDef { _nm = "coq_true"  }, _cparams = [] } = CExprBool True
boolSemantics CExprCall { _cd = CDef { _nm = "coq_false" }, _cparams = [] } = CExprBool False
boolSemantics other = omap boolSemantics other
