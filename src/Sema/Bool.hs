{-# LANGUAGE RecordWildCards, OverloadedStrings  #-}
module Sema.Bool where
import Sema.Common
import Codegen.Expr
import Data.Text (Text)
import qualified Data.Text as T

boolSemantics :: CExpr -> CExpr
-- Semantics for True and False
boolSemantics CExprCall { _fname = "Datatypes.Coq_true", _fparams = [] } = CExprBool True
boolSemantics CExprCall { _fname = "Datatypes.Coq_false", _fparams = [] } = CExprBool False
boolSemantics CExprCall { .. } = CExprCall _fname (map boolSemantics _fparams)
-- TODO: add more bool expression semantics
boolSemantics other = descend boolSemantics other

