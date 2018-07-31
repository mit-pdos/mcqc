{-# LANGUAGE RecordWildCards, OverloadedStrings  #-}
module Sema.Byte where
import Sema.Common
import Codegen.Expr
import Data.Bits

-- Ascii as byte semantics
asciiSemantics :: CExpr -> CExpr
asciiSemantics CExprCall { _fname = "Ascii.Ascii", _fparams = fp }
    | (length fp) == 8 = CExprChar $ makeByte (map _bool fp)
    | otherwise        = error "Ascii char is not 8 bytes"
    where fromBool b      = if b then fromInteger 1 else zeroBits
          makeByte []     = zeroBits
          makeByte (b:bs) = (fromBool b) `shift` (7 - (length bs)) .|. (makeByte bs)
asciiSemantics CExprCall { .. } = CExprCall _fname (map asciiSemantics _fparams)
asciiSemantics other = descend asciiSemantics other

