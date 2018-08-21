{-# LANGUAGE RecordWildCards, OverloadedStrings  #-}
module Sema.Bool where
import Common.Flatten
import Codegen.Expr

-- TODO: add more bool expression semantics
boolSemantics :: CExpr -> CExpr
-- Semantics for True and False
boolSemantics CExprCall { _fname = "Datatypes.Coq_true", _fparams = [] } = CExprBool True
boolSemantics CExprCall { _fname = "Datatypes.Coq_false", _fparams = [] } = CExprBool False
boolSemantics other = descend boolSemantics other

