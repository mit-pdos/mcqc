{-# LANGUAGE OverloadedStrings  #-}
module Sema.Pair where
import Data.MonoTraversable
import CIR.Expr

-- Pair semantics
pairSemantics :: CExpr -> CExpr
pairSemantics CExprCall { _cd = CDef { _nm = "coq_pair" }, _cparams = [a,b] } =
    CExprPair (pairSemantics a) (pairSemantics b)
pairSemantics CExprCall { _cd = CDef { _nm = "coq_pair" }, _cparams = bad }  =
    error $ "Datatypes.Coq_pair with not-two args found " ++ show bad
pairSemantics other = omap pairSemantics other

