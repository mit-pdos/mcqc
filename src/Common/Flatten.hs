{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Common.Flatten where
import Parser.Expr
import Codegen.Expr
import Codegen.Decl
import Codegen.Defs
import Data.Text (Text)
import Control.Lens

-- Traverse AST for all typenames
getTypes :: CExpr -> [Text]
getTypes CExprSeq    { .. } = "proc":(getTypes _left ++ getTypes _right)
getTypes CExprCall   { _fname  = "Datatypes.Some", .. } = "optional":(concat $ map getTypes _fparams)
getTypes CExprCall   { _fname  = "Datatypes.None", .. } = "optional":(concat $ map getTypes _fparams)
getTypes CExprCall   { .. } = _fname:(concat $ map getTypes _fparams)
getTypes CExprStr    { .. } = ["string"]
getTypes CExprNat    { .. } = ["nat"]
getTypes CExprTuple  { .. } = "tuple":(concat $ map getTypes _items)
getTypes CExprStmt   { .. } = "proc":_stype:(getTypes _sbody)
getTypes CExprList   { .. } = "list":(concat $ map getTypes _elems)
getTypes CExprLambda { .. } = (map _typename _largs) ++ (getTypes _lbody)
getTypes CExprVar    { .. } = []

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

