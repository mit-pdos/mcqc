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
                             <> line
                             <> (vcat . indentCases .  init) _cases  -- add a comma in intermediate cases
                             <> (tab . pretty . last) _cases         -- not on the last one though
                             <> ")"
    where indentCases = map $ \p -> tab (pretty p <> ",")
                             <> line
  pretty CExprMatch   { .. } = "[=](" <> pretty _mpat
                             <> "){ return"
                             <+> pretty _mbody
                             <> ";" <+> "}"
  pretty CExprCall    { .. } = pretty _fname <> "(" <> commatize _fparams <> ")"
  pretty CExprStr     { .. } = pretty _str
  pretty CExprNat     { .. } = "(Nat)" <> pretty _nat
  pretty CExprBool    { .. } = pretty _bool
  pretty CExprList    { .. } = "List<T>{" <> commatize _elems <> "}"
  pretty CExprCtor    { .. } = commatize _cargs
  pretty CExprTuple   { .. } = "PLACEHOLDER FOR TUPLE CTOR" <> (parens . hsep $ map pretty _items)
  pretty CExprWild     {}     = "Otherwise"
