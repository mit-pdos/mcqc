{-# LANGUAGE OverloadedStrings  #-}
module Sema.Tuple where
import Data.MonoTraversable
import CIR.Expr

-- Tuple semantics
tupleSemantics :: CExpr -> CExpr
tupleSemantics CExprCall { _fname = "Datatypes.Coq_pair", _fparams = [a] }  =
    error "Datatypes.Coq_pair with one arg found, undefined behavior."
tupleSemantics CExprCall { _fname = "Datatypes.Coq_pair", _fparams = args } =
    CExprTuple $ map tupleSemantics args -- TODO: Type inference
tupleSemantics other =
    omap tupleSemantics other

