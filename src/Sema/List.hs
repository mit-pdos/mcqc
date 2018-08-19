{-# LANGUAGE RecordWildCards, OverloadedStrings  #-}
module Sema.List where
import Common.Flatten
import Codegen.Expr

-- List semantics
listSemantics :: CExpr -> CExpr
-- Semantics for O and S
listSemantics CExprCall { _fname = "Datatypes.Coq_nil", _fparams = [] }       = CExprList []
listSemantics CExprCall { _fname = "Datatypes.Coq_nil", _fparams = [args] }   = error "Datatypes.Coq_nil with args found!"
listSemantics CExprCall { _fname = "Datatypes.Coq_cons", _fparams = [a, b] }  = CExprList $ (listSemantics a):(_elems . listSemantics $ b)
listSemantics CExprCall { _fname = "Datatypes.Coq_cons", _fparams = a:b:arg } = error "Datatypes.Coq_cons with more than two args found!"
listSemantics other                                                           = descend listSemantics other
