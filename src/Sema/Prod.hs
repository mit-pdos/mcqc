{-# LANGUAGE OverloadedStrings  #-}
module Sema.Prod where
import Data.MonoTraversable
import CIR.Expr

-- Pair semantics
prodSemantics :: CExpr -> CExpr
prodSemantics CExprCall { _cd = CDef { _nm = "coq_pair" }, _cparams = [a,b] } =
    CExprProd (prodSemantics a) (prodSemantics b)
prodSemantics CExprCall { _cd = CDef { _nm = "coq_pair" }, _cparams = bad }  =
    error $ "Datatypes.Coq_pair with not-two args found " ++ show bad
prodSemantics other = omap prodSemantics other

