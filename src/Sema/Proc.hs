{-# LANGUAGE RecordWildCards, OverloadedStrings  #-}
module Sema.Proc where
import Common.Flatten
import qualified Data.Text as T
import CIR.Expr
import Debug.Trace

-- Proc semantics (monadic)
bindSemantics :: CExpr -> CExpr
-- bindSemantics s | trace ("DBG Sema/Proc.hs/bindSemantics " ++ (show s)) False = undefined
bindSemantics CExprCall { _fname = "MProc.Proc.Coq_ret", _fparams = [a] } = a
bindSemantics CExprCall { _fname = "MProc.Proc.Coq_bind", _fparams = [call, CExprLambda { _largs = [varname], .. }] } =
    CExprSeq statement $ bindSemantics _lbody
    where statement = CExprStmt CTAuto varname $ bindSemantics call
bindSemantics other                                                 =
    descend bindSemantics other

-- Remove native type instances (since Coq extracts them as arguments)
removeInstances :: CExpr -> CExpr
removeInstances CExprCall { _fparams = (v@CExprVar { .. }:ts), .. }
    | T.isPrefixOf "native" lastthing = CExprCall _fname $ map removeInstances ts
    | T.isPrefixOf "show" lastthing = CExprCall _fname $ map removeInstances ts
    | otherwise = CExprCall _fname $ map removeInstances (v:ts)
    where lastthing = last $ T.splitOn "." _var
removeInstances other = descend removeInstances other

procSemantics :: CExpr -> CExpr
procSemantics = removeInstances . bindSemantics
