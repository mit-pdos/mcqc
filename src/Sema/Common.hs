{-# LANGUAGE RecordWildCards #-}
module Sema.Common where
import Codegen.Expr

{-
data CExpr =
            CExprLambda { _largs :: [CDef], _lbody :: CExpr }
          | CExprCase { _cexpr :: CExpr, _cases :: [CExpr] }
          | CExprMatch { _mpat :: CExpr, _mbody :: CExpr }    -- Matched to Case
          | CExprCall { _fname :: Text, _fparams :: [CExpr] } -- Use this for function calls and constructors
          | CExprStmt { _stype :: Text, _sname :: Text, _sbody :: CExpr } -- C++ statament for monadic unrolling
          -- Patterns
          | CExprCtor { _cname :: Text, _cargs :: [CDef] }
          | CExprTuple { _items :: [CExpr] }
          | CExprWild {}                                      -- Wildcard pattern, matches everything
          -- Reduced forms
          -- TODO: Add optional and nested types
          | CExprStr { _str :: Text }
          | CExprNat { _nat :: Int }
          | CExprBool { _bool :: Bool }
          | CExprList { _elems :: [CExpr] }
          -- Continuation
          | CExprSeq { _left :: CExpr, _right :: CExpr }
-}

-- Propagate to children expr
descend :: (CExpr -> CExpr) -> CExpr -> CExpr
descend f c@CExprCall   { .. } = f c
descend f   CExprStmt   { .. } = CExprStmt _stype _sname (descend f _sbody)
descend f   CExprLambda { .. } = CExprLambda _largs (f _lbody)
descend f   CExprCase   { .. } = CExprCase (f _cexpr) (map f _cases)
descend f   CExprMatch  { .. } = CExprMatch (f _mpat) (f _mbody)
descend f   CExprTuple  { .. } = CExprTuple (map f _items)
descend f   CExprList   { .. } = CExprList $ map (descend f) _elems
-- If it doesn't match anything, then it's a normal form, ignore
descend f   other              = other
