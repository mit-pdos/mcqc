{-# LANGUAGE RecordWildCards, OverloadedStrings  #-}
module Sema.Proc where
import qualified Data.Text as T
import Common.Utils
import CIR.Expr
import Data.MonoTraversable
import Debug.Trace

-- Proc semantics (monadic)
bindSemantics :: CExpr -> CExpr
-- bindSemantics s | trace ("DBG Sema/Proc.hs/bindSemantics " ++ (show s)) False = undefined
bindSemantics CExprCall { _cd = CDef { _nm = "MProc.Proc.Coq_ret"  }, _cparams = [a] } = a
bindSemantics CExprCall { _cd = CDef { _nm = "MProc.Proc.Coq_bind" }, _cparams = [call, CExprLambda { _lds = [CDef { .. }], .. }] } =
    CExprSeq statement $ bindSemantics _lbody
    where statement = CExprStmt (mkdef _nm) $ bindSemantics call
bindSemantics other = omap bindSemantics other

-- Remove native type instances (since Coq extracts them as arguments)
removeInstances :: CExpr -> CExpr
removeInstances CExprCall { _cparams = (v@CExprVar { .. }:ts), .. }
    | T.isPrefixOf "native" lastthing = CExprCall _cd $ map removeInstances ts
    | T.isPrefixOf "Native" lastthing = CExprCall _cd $ map removeInstances ts
    | T.isPrefixOf "show" lastthing   = CExprCall _cd $ map removeInstances ts
    | otherwise = CExprCall _cd $ map removeInstances (v:ts)
    where lastthing = last $ T.splitOn "." _var
removeInstances other = omap removeInstances other

procSemantics :: CExpr -> CExpr
procSemantics = removeInstances . bindSemantics
