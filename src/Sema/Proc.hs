{-# LANGUAGE RecordWildCards, OverloadedStrings  #-}
module Sema.Proc where
import Sema.Common
import Codegen.Expr
import Codegen.Defs
import Data.Text (Text)
import qualified Data.Text as T

-- Proc semantics (monadic)
procSemantics :: CExpr -> CExpr
procSemantics CExprCall { _fname = "Coq_bind", _fparams = [arg] }   = error "Datatypes.Coq_bind with one arg found, undefined behavior."
procSemantics CExprCall { _fname = "Coq_bind", _fparams = [a,
    CExprLambda { .. }] }                                           = CExprSeq (CExprStmt typenm nm a) $ procSemantics _lbody
    where unidef []     = ("void", "fake_name")
          unidef [d@CDef { .. }]    = (_typename, _name)
          unidef (a:ls) = error "Bind to multiple arguments not allowed, failing"
          typenm = fst . unidef $ _largs
          nm = snd . unidef $ _largs
procSemantics CExprCall { _fname = "Coq_bind", _fparams = a:b:arg } = error "Datatypes.Coq_bind with more than two args found!"
procSemantics CExprCall { _fname = "Coq_ret", .. }                  = CExprStr "return" -- TODO: implement return arguments
procSemantics CExprCall { .. }                                      = CExprCall _fname (map procSemantics _fparams)
procSemantics other                                                 = descend procSemantics other

