{-# LANGUAGE OverloadedStrings, FlexibleContexts, RecordWildCards #-}
module Optimizations.Flatten where
import Codegen.Rewrite
import Codegen.Defs
import Codegen.Expr
import Codegen.Pattern
import Parser.Expr
import Data.Text (Text)

-- WIP: Define a substitution function
subst :: CExpr -> Text -> CExpr -> CExpr
subst s var CExprLambda { .. } = CExprLambda largs (subst s var lbody)
subst s var CExprCase   { .. } = CExprCase (subst s var cexpr) (map (subst s var) cases)
subst s var CExprMatch  { .. } = CExprMatch mpat (subst s var mbody)
subst s var CExprCall   { .. } = CExprCall cfunc (map (subst s var) cparams)
subst s var s' = s

--flattenPatterns :: CExpr -> CExpr
--flattenPatterns CExprLambda { .. } = CExprLambda largs (flattenPatterns lbody)
--flattenPatterns CExprCase   { cases = (CExprMatch { body = CExprCase { cexpr = cexpr_innermost, cases = cases_innermost }:cs, .. } =
--	CExprCase cexpr

--(map flattenPatterns cases) ++ (collectPatterns cases)
--flattenPatterns CExprMatch  { mbody = m@CExprMatch { mpat = patin, mbody = bin }, .. } = CExprMatch mpat (flattenPatterns m)
--flattenPatterns CExprMatch  { .. } = CExprMatch mpat (flattenPatterns mbody)
--flattenPatterns s = s


collectPatterns :: [CExpr] -> [CExpr]
collectPatterns s = s
