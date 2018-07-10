{-# LANGUAGE DeriveAnyClass, DeriveGeneric, RecordWildCards, OverloadedStrings  #-}
module Codegen.Expr where
import GHC.Generics
import Codegen.Rewrite
import Codegen.Defs
import Codegen.Pattern
import Codegen.Utils
import Parser.Decl
import Parser.Expr
import Data.Aeson
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
          | CExprCtor { cname :: Text, cargs :: [CDef] }
          -- Reduced forms
          -- TODO: Add optional and nested types
          | CExprStr { str :: Text }
          | CExprNat { nat :: Int }
          | CExprListNat { nats :: [Int] }
          | CExprListStr { strs :: [Text] }
    deriving (Eq, Generic, ToJSON)

toCExpr :: Expr -> CExpr
toCExpr ExprLambda      { .. } = CExprStr "<PLACEHOLDER FOR LAMBDA>" -- CExprLambda (getCDefExtrap argtypes argnames) (toCExpr body)
toCExpr ExprCase        { .. } = CExprCase (toCExpr expr) (map caseCExpr cases)
toCExpr ExprConstructor { .. } = CExprCall (toCName name) (map toCExpr args)
toCExpr ExprApply       { func = ExprGlobal { .. }, .. } = CExprCall (toCName name) (map toCExpr args)
toCExpr ExprApply       { func = ExprRel    { .. }, .. } = CExprCall (toCName name) (map toCExpr args)
toCExpr ExprRel         { .. } = CExprStr (toCName name) -- TODO: Distinguish between Rel and Global
toCExpr ExprGlobal      { .. } = CExprStr (toCName name)

-- Case to CExpr
caseCExpr :: Case -> CExpr
caseCExpr Case { .. } = CExprMatch (toCPattern pat) (toCExpr body)

-- // Functional fibonacci
-- static inline constexpr Nat fib(Nat &a) {
--     return match(a,
--         []()      { return (Nat)1; },
--         [](Nat sm) {
--           return match(sm,
--             []()       { return (Nat)1; },
--             [sm](Nat m) { return add(fib(m), fib(sm)); });
--         });
-- }
instance Pretty CExpr where
  pretty CExprLambda  { .. } = "<PLACEHOLDER FOR LAMBDA>" :: Doc ann
  pretty CExprCase    { .. } = "match(" <> pretty cexpr <> ","
                             <> hardline
                             <> (tab . vcat . (map (\p -> p <> "," <> hardline)) . (map pretty) . init) cases  -- add a comma in intermediate cases
                             <> (pretty . last) cases                                                    -- not on the last one though
                             <> ");"
                             <> hardline
  pretty CExprMatch   { .. } = "[](" <> pretty mpat
                             <+> "){ return"
                             <+> pretty mbody
  pretty CExprCall    { .. } = mkFuncSig cfunc (map pretty cparams)
  pretty CExprStr     { .. } = pretty . toCName $ str
  pretty CExprNat     { .. } = pretty nat
  pretty CExprStr     { .. } = pretty . toCName $ str
  pretty CExprListNat { .. } = "PLACEHOLDER FOR INLINE List<int>(" <+> concatWith (surround ", ") (map pretty nats) <> ");"
  pretty CExprListStr { .. } = "PLACEHOLDER FOR INLINE List<String>(" <+> concatWith (surround ", ") (map pretty strs) <> ");"
  pretty CExprCtor    { .. } = "Constructor" <> pretty cname
                             <+> "return"
                             <+> hcat (map pretty cargs)

