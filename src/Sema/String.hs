{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Sema.String where
import CIR.Expr
import Common.Utils
import Data.MonoTraversable
import Data.Text (Text)
import qualified Data.Text as T
import Data.Bits

-- Ascii as byte semantics
asciiSemantics :: CExpr -> CExpr
asciiSemantics CExprCall { _cd = CDef { _nm = "Ascii.Ascii" }, _cparams = fp }
    | length fp == 8 = CExprStr $ T.pack [w2c (makeByte (map _bool fp))]
    | otherwise      = error "Ascii char is not 8 bytes"
    where fromBool b      = if b then 1 else zeroBits
          makeByte []     = zeroBits
          makeByte (b:bs) = fromBool b `shift` (7 - length bs) .|. makeByte bs
asciiSemantics other = omap asciiSemantics other

-- Parse strings
serialize :: CExpr -> (Maybe Text, Maybe CExpr)
serialize CExprCall { _cd = CDef { _nm = "String" }, _cparams = [CExprStr { .. }, e] } =
    case serialize e of
      (Just estr, Nothing) -> (Just $ _str `T.append` estr, Nothing)
      (Just estr, Just e)  -> (Just $ _str `T.append` estr, Just $ stringSemantics e)
      (Nothing, Just e)    -> (Nothing, Just $ stringSemantics e)
      (Nothing, Nothing)   -> (Nothing, Just $ stringSemantics e)
serialize CExprCall { _cd = CDef { _nm = "EmptyString" } } = (Just "", Nothing)
serialize e = (Nothing, Just e)

-- Handle string semantics
stringSemantics :: CExpr -> CExpr
stringSemantics e =
    case serialize e of
      (Just str, Nothing) -> CExprStr str
      (Just str, Just e)  -> CExprCall (CDef "append" (CTBase "string")) [CExprStr str, omap stringSemantics e]
      (Nothing, Just e)   -> omap stringSemantics e
      (Nothing, Nothing)  -> omap stringSemantics e

