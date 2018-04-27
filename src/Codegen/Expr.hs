{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Codegen.Expr (CExpr, toCExpr) where
import Codegen.Rewrite (toCType, toCName)
import Codegen.Defs
import Parser.Decl
import Parser.Expr

-- C++ Expressions
data CExpr =
            CExprLambda { largs :: [CDef], lbody :: CExpr }
          | CExprCase { cexpr :: CExpr, cases :: [CExpr] }
          | CExprCall { cfunc :: String, cparams :: [CExpr] } -- Reuse this for function calls and constructors
          | CExprGlobal { gname :: String }
          | CExprPat { cpat :: CExpr, body :: CExpr }
    deriving (Eq)

-- TODO: Implement expression rewritting
toCExpr :: Expr -> CExpr
toCExpr e = CExprGlobal (show e)

-- Pretty print C++ from these types
instance Show CExpr where
  show CExprLambda  { .. } = "<PLACEHOLDER FOR LAMBDA>"
  show CExprCase    { .. } = "\tMatch(" ++ (show cexpr) ++ "){\n"
                             ++ (unlines ccases) ++ "\n"
                             ++ "}\nEndMatch\n"
    where ccases = map (("\t\t" ++) . show) cases
  show CExprCall    { .. } = cfunc ++ "(" ++ (mkparams cparams) ++ ")"
    where mkparams []     = ""
          mkparams [e]    = show e
          mkparams (e:el) = (show e) ++ ", " ++ (mkparams el)
  show CExprGlobal  { .. } = gname
  show CExprPat     { .. } = "When(" ++ (show cpat) ++")\t\t return " ++ (show body) ++ ";"
