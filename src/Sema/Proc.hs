{-# LANGUAGE RecordWildCards, OverloadedStrings  #-}
module Sema.Proc where
import Common.Flatten
import Common.Predicates(isSeq, isRet)
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
bindSemantics CExprCall { _fname = "MProc.Proc.Coq_bind", _fparams = [arg] }   =
    error "Bind with one arg found, undefined behavior."
bindSemantics CExprCall { _fname = "MProc.Proc.Coq_bind", _fparams = [call, CExprLambda { _largs = args, .. }] } =
    error "Bind followed by a non-unary lambda, undefined behavior"
bindSemantics CExprCall { _fname = "MProc.Proc.Coq_bind", _fparams = a:b:arg } =
    error "Bind with more than two args found, undefined behavior"
bindSemantics other                                                 =
    descend bindSemantics other

seqSemantics :: CExpr -> CExpr
-- Wrap an imperative sequence in a lambda, making it a first-class object
seqSemantics CExprCall { .. } = CExprCall _fname $ map mkLambda _fparams
    where mkLambda CExprSeq { .. } = CExprLambda [] $ CExprSeq (seqSemantics _left) (seqSemantics _right)
          mkLambda other  = other
-- Make sure an imperative sequence returns
seqSemantics CExprSeq { .. }
    | isSeq _right = CExprSeq _left $ seqSemantics _right
    | isRet _right = CExprSeq _left _right
    | otherwise    = CExprSeq _left $ CExprCall "return" [seqSemantics _right]
seqSemantics other = descend seqSemantics other

-- Remove native type instances (since Coq extracts them as arguments)
removeInstances :: CExpr -> CExpr
removeInstances c | trace ("DBG Removing instance " ++ show c) False = undefined
removeInstances CExprCall { _fparams = (v@CExprVar { .. }:ts), .. }
    | T.isPrefixOf "native" lastthing = CExprCall _fname $ map removeInstances ts
    | otherwise = CExprCall _fname $ map removeInstances (v:ts)
    where lastthing = last $ T.splitOn "." _var
removeInstances other = descend removeInstances other

procSemantics :: CExpr -> CExpr
procSemantics = removeInstances . seqSemantics . bindSemantics
