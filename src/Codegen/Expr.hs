{-# LANGUAGE OverloadedStrings, FlexibleContexts, RecordWildCards #-}
module Codegen.Expr where
import Codegen.Rewrite
import Codegen.Defs
import Codegen.Pattern
import Codegen.Utils
import Parser.Decl
import Parser.Expr
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text

-- C++ Expressions
data CExpr =
            CExprLambda { largs :: [CDef], lbody :: CExpr }
          | CExprCase { cexpr :: CExpr, cases :: [CExpr] }
          | CExprMatch { mpat :: CPattern, mbody :: CExpr } -- Matched to Case
          | CExprCall { cfunc :: Text, cparams :: [CExpr] } -- Use this for function calls and constructors
          | CExprRel    { rname :: Text }
          | CExprGlobal { gname :: Text }
          | CExprCtor { cname :: Text, cargs :: [CDef] }
    deriving (Eq)

-- Expression rewritting
toCExpr :: Expr -> CExpr
toCExpr ExprLambda      { .. } = CExprGlobal "<PLACEHOLDER FOR LAMBDA>" -- CExprLambda (getCDefExtrap argtypes argnames) (toCExpr body)
toCExpr ExprCase        { .. } = CExprCase (toCExpr expr) (map caseToCExpr cases)
toCExpr ExprConstructor { .. } = CExprCall (toCName name) (map toCExpr args)
toCExpr ExprApply       { func = ExprGlobal { .. }, args = al@[l, r]} = CExprCall (toCName name) (map toCExpr al)
toCExpr ExprApply       { func = ExprRel    { .. }, args = al@[l, r]} = CExprCall (toCName name) (map toCExpr al)
toCExpr ExprApply       { func = ExprGlobal { .. }, args = al } = CExprCall (toCName name) (map toCExpr al)
toCExpr ExprRel         { .. } = CExprRel (toCName name)
toCExpr ExprGlobal      { .. } = CExprGlobal (toCName name)
-- Cases
caseToCExpr :: Case -> CExpr
caseToCExpr Case        { .. } = CExprMatch (toCPattern pat) (toCExpr body)

instance Pretty CExpr where
  pretty CExprLambda  { .. } = "<PLACEHOLDER FOR LAMBDA>" :: Doc ann
  pretty CExprCase    { .. } = line
                             <> "Match" <+> (maybeParens . pretty) cexpr <+> "{"
                             <> hardline
                             <> (tab . vcat) (map pretty cases)
                             <> "}"
                             <> hardline
  pretty CExprMatch   { .. } = hcat $ ["When", (maybeParens . pretty) mpat, "\t", "return ", pretty mbody, ";", hardline]
  pretty CExprCall    { .. } = mkFuncSig cfunc (map pretty cparams)
  pretty CExprRel     { .. } = pretty . toCName $ rname
  pretty CExprGlobal  { .. } = pretty . toCName $ gname
  pretty CExprCtor    { .. } = hcat $ ["C", (maybeParens . pretty) cname, "\t", "return "] ++ (map pretty cargs)

