{-# LANGUAGE OverloadedStrings  #-}
module Sema.Tuple where
import Data.MonoTraversable
import CIR.Expr

-- Tuple semantics
tupleSemantics :: CExpr -> CExpr
tupleSemantics CExprCall { _cd = CDef { _nm = "Datatypes.Coq_pair" }, _cparams = [a] }  =
    error "Datatypes.Coq_pair with one arg found, undefined behavior."
tupleSemantics CExprCall { _cd = CDef { _nm = "Datatypes.Coq_pair" }, _cparams = args } =
    CExprTuple $ map tupleSemantics args
tupleSemantics other = omap tupleSemantics other

