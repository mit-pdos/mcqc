{-# LANGUAGE RecordWildCards, OverloadedStrings  #-}
module Sema.String where
import Sema.Common
import Codegen.Expr
import Codegen.Utils
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

makeStr :: CExpr -> Text
makeStr CExprStr  { .. } = _str
makeStr CExprCall { _fname = "String.EmptyString", _fparams = [] } = mempty
makeStr CExprCall { _fname = "String.EmptyString", .. } = error "EmptyString takes no arguments"

stringSemantics :: CExpr -> CExpr
stringSemantics CExprCall { _fname = "String.String", .. } =
    CExprStr $ T.concat $ map makeStr _fparams
stringSemantics s@CExprCall { _fname = "String.EmptyString" } = CExprStr $ makeStr s
stringSemantics CExprCall   { .. } = CExprCall _fname (map stringSemantics _fparams)
stringSemantics CExprLambda { .. } = CExprLambda _largs $ stringSemantics _lbody
stringSemantics other = descend stringSemantics other
