{-# LANGUAGE RecordWildCards, OverloadedStrings  #-}
module PrettyPrinter.Expr where
import Codegen.Expr
import Codegen.Utils
import PrettyPrinter.Defs
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text

instance Pretty CExpr where
  pretty CExprLambda  { .. } = "<PLACEHOLDER FOR LAMBDA>" :: Doc ann
  pretty CExprCase    { .. } = "match(" <> pretty _cexpr <> ","
                             <> hardline
                             <> (vcat . indentCases .  init) _cases  -- add a comma in intermediate cases
                             <> (tab . pretty . last) _cases         -- not on the last one though
                             <> ")"
    where indentCases = map $ \p -> tab (pretty p <> ",") <> hardline
  pretty CExprMatch   { .. } = "[=](" <> pretty _mpat
                             <+> "){ return"
                             <+> pretty _mbody
                             <> "; }"
  pretty CExprCall    { .. } = mkFuncSig _fname (map pretty _fparams)
  pretty CExprStr     { .. } = pretty _str
  pretty CExprNat     { .. } = "(Nat)" <> pretty _nat
  pretty CExprListNat { .. } = "PLACEHOLDER FOR INLINE List<int>(" <+> concatWith (surround ", ") (map pretty _nats) <> ");"
  pretty CExprListStr { .. } = "PLACEHOLDER FOR INLINE List<String>(" <+> concatWith (surround ", ") (map pretty _strs) <> ");"
  pretty CExprCtor    { .. } = hcat $ map pretty  _cargs
  pretty CExprTuple   { .. } = "PLACEHOLDER FOR TUPLE CTOR" <> (parens . hsep $ map pretty _items)
  pretty CExprWild     {}     = "Otherwise"
