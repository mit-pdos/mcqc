{-# LANGUAGE RecordWildCards #-}
module Sema.Common where
import Codegen.Expr

-- Propagate to children expr
descend :: (CExpr -> CExpr) -> CExpr -> CExpr
descend f c@CExprCall   { .. } = f c
descend f   CExprStmt   { .. } = CExprStmt _stype _sname $ descend f _sbody
descend f   CExprLambda { .. } = CExprLambda _largs $ descend f _lbody
descend f   CExprSeq    { .. } = CExprSeq (descend f _left) (descend f _right)
descend f   CExprTuple  { .. } = CExprTuple $ map (descend f) _items
descend f   CExprList   { .. } = CExprList $ map (descend f) _elems
-- If it doesn't match anything, then it's a normal form, ignore
descend f   other              = other
