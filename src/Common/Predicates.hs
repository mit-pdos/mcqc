{-# language OverloadedStrings, RecordWildCards #-}
module Common.Predicates where
import Codegen.Expr

isSeq :: CExpr -> Bool
isSeq CExprSeq { .. } = True
isSeq _               = False

isRet :: CExpr -> Bool
isRet CExprCall { _fname = "Coq_ret", .. } = True
isRet CExprCall { _fname = "return", .. }  = True
isRet _                                    = False

