{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Codegen.Expr (CExpr, toCExpr) where
import Codegen.Rewrite (toCType, toCName)
import Codegen.Defs
import Codegen.Pattern
import Parser.Decl
import Parser.Expr

-- C++ Expressions
data CExpr =
            CExprLambda { largs :: [CDef], lbody :: CExpr }
          | CExprCase { cexpr :: CExpr, cases :: [CExpr] }
          | CExprMatch { mpat :: CPattern, mbody :: CExpr } -- Matched to Case
          | CExprCall { cfunc :: String, cparams :: [CExpr] } -- Use this for function calls and constructors
          | CExprRel    { rname :: String }
          | CExprGlobal { gname :: String }
          | CExprCtor { cname :: String, cargs :: [CDef] }
    deriving (Eq)

-- Expression rewritting
toCExpr :: Expr -> CExpr
toCExpr ExprLambda      { .. } = CExprGlobal "<PLACEHOLDER FOR LAMBDA>" -- CExprLambda (getCDefExtrap argtypes argnames) (toCExpr body)
toCExpr ExprCase        { .. } = CExprCase (toCExpr expr) (map caseToCExpr cases)
toCExpr ExprConstructor { .. } = CExprCall (toCName name) (map toCExpr args)
toCExpr ExprApply       { func = ExprGlobal { .. }, .. } = CExprCall (toCName name) (map toCExpr args)
toCExpr ExprApply       { func = ExprRel { .. }, .. } = CExprCall (toCName name) (map toCExpr args)
toCExpr ExprRel         { .. } = CExprRel (toCName name)
toCExpr ExprGlobal      { .. } = CExprGlobal (toCName name)
-- Cases
caseToCExpr :: Case -> CExpr
caseToCExpr Case        { .. } = CExprMatch (toCPattern pat) (toCExpr body)

-- Pretty print C++ from these types
instance Show CExpr where
  show CExprLambda  { .. } = "<PLACEHOLDER FOR LAMBDA>"
  show CExprCase    { .. } = "\tMatch(" ++ (show cexpr) ++ "){\n"
                             ++ (unlines ccases) ++ "\n"
                             ++ "}\nEndMatch\n"
    where ccases = map (("\t\t" ++) . show) cases
  show CExprMatch   { .. } = "match(" ++ (show mpat) ++ ")\treturn " ++ (show mbody) ++";"
  show CExprCall    { .. } = cfunc ++ "(" ++ (mkparams cparams) ++ ")"
    where mkparams []     = ""
          mkparams [e]    = show e
          mkparams (e:el) = (show e) ++ ", " ++ (mkparams el)
  show CExprRel     { .. } = (toCName rname)
  show CExprGlobal  { .. } = (toCName gname)
  show CExprCtor    { .. } = "ctor(" ++ cname ++ ")\treturn " ++ (show cargs) ++";"
