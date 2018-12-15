{-# LANGUAGE RecordWildCards, OverloadedStrings  #-}
module Sema.String where
import CIR.Expr
import Common.Utils
import Data.Text (Text)
import Data.MonoTraversable
import qualified Data.Text as T
import Data.Bits
import Debug.Trace

-- Ascii as byte semantics
asciiSemantics :: CExpr -> CExpr
asciiSemantics CExprCall { _cd = CDef { _nm = "Ascii.Ascii" }, _cparams = fp }
    | length fp == 8 = CExprStr $ T.pack [w2c (makeByte (map _bool fp))]
    | otherwise      = error "Ascii char is not 8 bytes"
    where fromBool b      = if b then 1 else zeroBits
          makeByte []     = zeroBits
          makeByte (b:bs) = fromBool b `shift` (7 - length bs) .|. makeByte bs
asciiSemantics other = omap asciiSemantics other

stringSemantics :: CExpr -> CExpr
-- Handle String(char c, string s) constructor
stringSemantics CExprCall { _cd = CDef { _nm = "String.String" }, _cparams = [char, str] } =
    case (char, str) of
        (CExprStr { _str = h }, CExprStr { _str = tl }) -> CExprStr $ h `T.append` tl
        (CExprStr { _str = h }, CExprCall { _cd = CDef { _nm = "String.EmptyString" }, .. }) -> CExprStr h
        (_, _) -> CExprCall (CDef "mkstring" strT) [char, str]
    where strT = CTBase "string"
stringSemantics CExprCall { _cd = CDef { _nm = "String.EmptyString" }, _cparams = [] } =
    CExprStr mempty
stringSemantics CExprCall { _cd = CDef { _nm = "String.EmptyString" }, .. } =
    error "EmptyString takes no arguments"
stringSemantics other = omap stringSemantics other

