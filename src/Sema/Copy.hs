{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards  #-}
module Sema.Copy where
import Common.Flatten
import Common.Config
import CIR.Expr

-- Copy semantics
copySemantics :: CExpr -> CExpr
copySemantics CExprCall { _fparams = CExprVar { .. }:ts, .. }
    | _fname `elem` mutables = if appearsany _var ts
                               -- copy a variable that appears >once in a mutable expression
                               then CExprCall _fname $ (CExprCall "copy" [CExprVar _var]):map copySemantics ts
                               -- Otherwise, it can be mutated safely
                               else CExprCall _fname $ (CExprVar _var):map copySemantics ts
    -- Predicates that detect a name in subexpressions
    where appearsany name el = foldr (||) False $ map (appearsin _var) el
          appearsin name CExprVar  { .. }
              | name == _var = True
              | otherwise    = False
          appearsin name CExprCall   { .. } = appearsany name _fparams
          -- if shadowing happens, do not copy
          appearsin name CExprLambda { .. } = appearsin name _lbody && not (name `elem` _largs)
          appearsin name CExprSeq    { .. } = appearsin name _left || appearsin name _right
          appearsin name CExprStmt   { .. } = appearsin name _sbody
          appearsin name CExprTuple  { .. } = appearsany name _items
          appearsin name o = False
copySemantics o = descend copySemantics o
