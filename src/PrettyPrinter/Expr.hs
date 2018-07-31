{-# LANGUAGE RecordWildCards, OverloadedStrings  #-}
module PrettyPrinter.Expr where
import Codegen.Expr
import Codegen.Utils
import Codegen.Rewrite
import PrettyPrinter.Defs
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Char as C
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
--          | CExprStmt { _stype :: Text, _sname :: Text, _sbody :: CExpr } -- C++ statament for monadic unrolling

instance Pretty CExpr where
  pretty CExprLambda  { .. } = pretty _largs
                             <> line
                             <+> pretty _lbody
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
  pretty CExprCall    { .. } = pretty (toCName _fname) <> "(" <> commatize _fparams <> ")"
--  pretty CExprChar    { .. } = "\"" <> pretty (w2c _char) <> "\""
  pretty CExprVar     { .. } = pretty _var
  pretty CExprStr     { .. } = "\"" <> pretty _str <> "\""
  pretty CExprNat     { .. } = "(Nat)" <> pretty _nat
  pretty CExprBool    { .. } = pretty _bool
  pretty CExprList    { .. } = "List<T>{" <> commatize _elems <> "}"
  pretty CExprCtor    { .. } = commatize _cargs
  pretty CExprTuple   { .. } = "PLACEHOLDER FOR TUPLE CTOR" <> (parens . hsep $ map pretty _items)
  pretty CExprStmt    { .. } = pretty _stype <+> pretty _sname <+> "=" <+> pretty _sbody
  pretty CExprSeq     { .. } = pretty _left <> ";" <> line <> pretty _right
  pretty CExprWild     {}     = "Otherwise"
