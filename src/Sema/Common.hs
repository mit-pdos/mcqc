{-# LANGUAGE RecordWildCards #-}
module Sema.Common where
import Codegen.Expr
import Codegen.Decl
import Control.Lens

-- Propagate to children expr
descend :: (CExpr -> CExpr) -> CExpr -> CExpr
descend f   CExprCall   { .. } = CExprCall _fname $ map f _fparams
descend f   CExprStmt   { .. } = CExprStmt _stype _sname $ f _sbody
descend f   CExprSeq    { .. } = CExprSeq (f _left) (f _right)
descend f   CExprTuple  { .. } = CExprTuple $ map f _items
descend f   CExprList   { .. } = CExprList $ map f _elems
descend f   CExprLambda { .. } = CExprLambda _largs $ f _lbody
-- If it doesn't match anything, then it's a normal form, ignore
descend f   other              = other

-- Propagate to children expr
descendF :: (CExpr -> CExpr) -> CDecl -> CDecl
descendF f = over fbody (descend f)

