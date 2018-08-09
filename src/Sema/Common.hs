{-# LANGUAGE RecordWildCards #-}
module Sema.Common where
import Codegen.Expr

-- Propagate to children expr
descend :: (CExpr -> CExpr) -> CExpr -> CExpr
descend f c@CExprCall   { .. } = f c
descend f   CExprStmt   { .. } = CExprStmt _stype _sname $ f _sbody
descend f   CExprSeq    { .. } = CExprSeq (f _left) (f _right)
descend f   CExprTuple  { .. } = CExprTuple $ map f _items
descend f   CExprList   { .. } = CExprList $ map f _elems
-- If it doesn't match anything, then it's a normal form, ignore
descend f   other              = other
