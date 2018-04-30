{-# LANGUAGE OverloadedStrings, FlexibleContexts, RecordWildCards #-}
module Optimizations.Flatten where
import Codegen.Rewrite
import Codegen.Defs
import Codegen.Expr
import Codegen.Pattern
import Parser.Expr
import Data.Text (Text)

-- WIP: Define a substitution function
subst :: Text -> CExpr -> CExpr
subst var CExprLambda { .. } = CExprLambda largs (subst var lbody)
subst var CExprCase   { .. } = CExprCase (subst var cexpr) (map (subst var) cases)
subst var CExprMatch  { .. } = CExprMatch mpat (subst var mbody)
subst var CExprCall   { .. } = CExprCall cfunc (map (subst var) cparams) isinfix
subst var s = s

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
