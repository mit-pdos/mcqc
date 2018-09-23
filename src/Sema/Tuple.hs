{-# LANGUAGE OverloadedStrings  #-}
module Sema.Tuple where
import Common.Flatten
import CIR.Expr

-- Proc semantics (monadic)
tupleSemantics :: CExpr -> CExpr
tupleSemantics CExprCall { _fname = "Datatypes.Coq_pair", _fparams = [a] }  =
    error "Datatypes.Coq_pair with one arg found, undefined behavior."
tupleSemantics CExprCall { _fname = "Datatypes.Coq_pair", _fparams = args } =
    CExprTuple $ map tupleSemantics args
tupleSemantics other =
    descend tupleSemantics other

