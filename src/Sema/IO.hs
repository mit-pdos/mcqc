{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Sema.IO where
import CIR.Expr
import Classes.Typeful
import Data.MonoTraversable
import qualified Data.Text as T

-- IO semantics (monadic)
bindSemantics :: CExpr -> CExpr
-- bindSemantics s | trace ("DBG Sema/IO.hs/bindSemantics " ++ (show s)) False = undefined
bindSemantics CExprCall { _cd = CDef { _nm = "coq_ret"  }, _cparams = [a] } = a
bindSemantics CExprCall { _cd = CDef { _nm = "coq_bind" }, _cparams = [call, CExprLambda { _lds = [CDef { .. }], .. }] } =
    CExprSeq statement $ bindSemantics _lbody
    where statement = CExprStmt (mkauto _nm) $ bindSemantics call
bindSemantics other = omap bindSemantics other

-- tt is the inhabitant of unit
ttSemantics :: CExpr -> CExpr
ttSemantics CExprCall { _cd = CDef { _nm = "coq_tt" }, _cparams = [] } = CExprVar ""
ttSemantics e = omap ttSemantics e

-- Remove native type instances (since Coq extracts them as arguments)
removeInstances :: CExpr -> CExpr
removeInstances CExprCall { _cparams = (v@CExprVar { .. }:ts), .. }
    | T.isPrefixOf "native" lastthing = CExprCall _cd $ map removeInstances ts
    | T.isPrefixOf "Native" lastthing = CExprCall _cd $ map removeInstances ts
    | T.isPrefixOf "show" lastthing   = CExprCall _cd $ map removeInstances ts
    | otherwise = CExprCall _cd $ map removeInstances (v:ts)
    where lastthing = last $ T.splitOn "." _var
removeInstances other = omap removeInstances other

-- Compile a Coq expression to a C Expression
retSemantics :: CExpr -> CExpr
retSemantics s@CExprSeq { .. } = listToSeq $ otherexpr s ++ [lastexpr s]
    where lastexpr  = mkreturn . last . seqToList
          otherexpr = map retSemantics . init . seqToList
          mkreturn e@CExprCall { _cd = CDef { _nm = "return" } } = e
          mkreturn e = CExprCall (mkauto "return") [e]
retSemantics e = omap retSemantics e

lambdaSemantics :: CExpr -> CExpr
lambdaSemantics s@CExprSeq { .. } = s
lambdaSemantics e@CExprCall { _cd = CDef { _nm = "return" } } = e
lambdaSemantics e = CExprCall (CDef "return" . gettype $ e) [e]

ioSemantics :: CExpr -> CExpr
ioSemantics = ttSemantics . lambdaSemantics . retSemantics . removeInstances . bindSemantics
