{-# LANGUAGE RecordWildCards, OverloadedStrings  #-}
module Sema.Proc where
import Sema.Common
import Codegen.Expr
import Codegen.Defs
import Data.Text (Text)
import qualified Data.Text as T

-- Proc semantics (monadic)
bindSemantics :: CExpr -> CExpr
bindSemantics CExprCall { _fname = "Coq_bind", _fparams = [arg] }   =
    error "Datatypes.Coq_bind with one arg found, undefined behavior."
bindSemantics CExprCall { _fname = "Coq_bind", _fparams = [call, CExprLambda { _largs = [d], .. }] } =
    CExprSeq statement $ bindSemantics _lbody
    where statement = CExprStmt (_typename d) (_name d) call
bindSemantics CExprCall { _fname = "Coq_bind", _fparams = a:b:arg } =
    error "Datatypes.Coq_bind with more than two args found!"
bindSemantics CExprCall { .. }                                      =
    CExprCall _fname (map bindSemantics _fparams)
bindSemantics other                                                 =
    descend bindSemantics other

procSemantics :: CExpr -> CExpr
procSemantics = bindSemantics
