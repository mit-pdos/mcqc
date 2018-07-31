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
bindSemantics CExprCall { _fname = "Coq_bind", _fparams = [a, CExprLambda { .. }] } =
    CExprSeq (CExprStmt typenm nm a) $ bindSemantics _lbody
    where unidef []     = ("void", "fake_name")
          unidef [d]    = (_typename d, _name d)
          unidef (a:ls) = error "Bind to multiple arguments not allowed, failing"
          typenm = fst . unidef $ _largs
          nm = snd . unidef $ _largs
bindSemantics CExprCall { _fname = "Coq_bind", _fparams = a:b:arg } =
    error "Datatypes.Coq_bind with more than two args found!"
bindSemantics CExprCall { _fname = "Coq_ret", .. }                  =
    CExprVar "return" -- TODO: implement return arguments
bindSemantics CExprCall { .. }                                      =
    CExprCall _fname (map bindSemantics _fparams)
bindSemantics other                                                 =
    descend bindSemantics other

-- Replace void assignments to simple function calls
stmtSemantics :: CExpr -> CExpr
stmtSemantics CExprStmt { _stype = "auto", _sname = "_", .. } = _sbody
stmtSemantics CExprSeq { .. }                = CExprSeq (stmtSemantics _left) (stmtSemantics _right)
stmtSemantics CExprCall { .. }               = CExprCall _fname (map stmtSemantics _fparams)
stmtSemantics other                          = descend stmtSemantics other

procSemantics :: CExpr -> CExpr
procSemantics = stmtSemantics . bindSemantics
