{-# LANGUAGE RecordWildCards, OverloadedStrings  #-}
module PrettyPrinter.Expr where
import Codegen.Expr
import Codegen.Utils
import PrettyPrinter.Defs
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text

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
  pretty CExprCase    { .. } = "match(" <> pretty _cexpr <> ","
                             <> hardline
                             <> (tab . vcat . (map (\p -> p <> "," <> hardline)) . (map pretty) . init) _cases  -- add a comma in intermediate cases
                             <> (pretty . last) _cases                                                          -- not on the last one though
                             <> ");"
                             <> hardline
  pretty CExprMatch   { .. } = "[](" <> pretty _mpat
                             <+> "){ return"
                             <+> pretty _mbody
  pretty CExprCall    { .. } = mkFuncSig _fname (map pretty _fparams)
  pretty CExprStr     { .. } = pretty _str
  pretty CExprNat     { .. } = pretty _nat
  pretty CExprListNat { .. } = "PLACEHOLDER FOR INLINE List<int>(" <+> concatWith (surround ", ") (map pretty _nats) <> ");"
  pretty CExprListStr { .. } = "PLACEHOLDER FOR INLINE List<String>(" <+> concatWith (surround ", ") (map pretty _strs) <> ");"
  pretty CExprCtor    { .. } = "Constructor" <> pretty _cname
                             <+> "return"
                             <+> hcat (map pretty _cargs)
  pretty CExprTuple   { .. } = "C" <> (parens . hsep $ map pretty _items)
  pretty CExprWild     {}     = "Otherwise"
