{-# LANGUAGE RecordWildCards, OverloadedStrings  #-}
module Sema.Tuple where
import Sema.Common
import Codegen.Expr

-- Proc semantics (monadic)
tupleSemantics :: CExpr -> CExpr
tupleSemantics CExprCall { _fname = "Coq_pair", _fparams = [a] }    =
    error "Datatypes.Coq_pair with one arg found, undefined behavior."
tupleSemantics CExprCall { _fname = "Coq_pair", _fparams = [a, b] } =
    CExprTuple [tupleSemantics a, tupleSemantics b]
tupleSemantics CExprCall { _fname = "Coq_pair", _fparams = args }   =
    CExprTuple (map tupleSemantics args)
tupleSemantics CExprCall { .. }                                     =
    CExprCall _fname (map tupleSemantics _fparams)
tupleSemantics other                                                =
    descend tupleSemantics other

