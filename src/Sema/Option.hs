{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards  #-}
module Sema.Option where
import CIR.Expr
import Data.MonoTraversable

-- Option semantics
optionSemantics :: CExpr -> CExpr
optionSemantics CExprCall { _cd = CDef { _nm = "Some", .. }, .. } =
    CExprCall (CDef "some" _ty) _cparams
optionSemantics CExprCall { _cd = CDef { _nm = "None", .. }, .. } =
    CExprCall (CDef "none" _ty) _cparams
optionSemantics other = omap optionSemantics other

