{-# LANGUAGE RecordWildCards, OverloadedStrings  #-}
module Sema.String where
import Common.Flatten
import CIR.Expr
import Common.Utils
import Data.Text (Text)
import qualified Data.Text as T
import Data.Bits

-- Ascii as byte semantics
asciiSemantics :: CExpr -> CExpr
asciiSemantics CExprCall { _fname = "Ascii.Ascii", _fparams = fp }
    | (length fp) == 8 = CExprStr $ T.pack [w2c (makeByte (map _bool fp))]
    | otherwise        = error "Ascii char is not 8 bytes"
    where fromBool b      = if b then fromInteger 1 else zeroBits
          makeByte []     = zeroBits
          makeByte (b:bs) = (fromBool b) `shift` (7 - (length bs)) .|. (makeByte bs)
asciiSemantics CExprCall { .. } = CExprCall _fname (map asciiSemantics _fparams)
asciiSemantics other = descend asciiSemantics other

stringSemantics :: CExpr -> CExpr
stringSemantics CExprCall { _fname = "String.String", .. } =
    CExprStr . T.concat $ map makeStr _fparams
    where makeStr CExprStr  { .. } = _str
          makeStr CExprCall { _fname = "String.EmptyString", .. } = ""
stringSemantics CExprCall { _fname = "String.EmptyString", _fparams = [] } =
    CExprStr mempty
stringSemantics CExprCall { _fname = "String.EmptyString", .. } =
    error "EmptyString takes no arguments"
stringSemantics other = descend stringSemantics other

