{-# LANGUAGE RecordWildCards, OverloadedStrings  #-}
module PrettyPrinter.Expr where
import Codegen.Expr
import Common.Utils
import Codegen.Rewrite
import PrettyPrinter.Defs
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Char as C
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text


instance Pretty CExpr where
  pretty CExprLambda  { _lbody = CExprSeq { .. }, .. } = group $ "[=](" <> commatize _largs <> ") {"
                             <> line <> tab (pretty _left <> ";" <> line <> pretty _right) <> ";"
                             <> line <> "}"
  pretty CExprLambda  { .. } = group $ "[=](" <> commatize _largs <> ") {"
                             <+> "return" <+> pretty _lbody <> ";"
                             <+> "}"
  pretty CExprCall    { _fname = "Coq_ret", _fparams = [a] } = group $ "return" <+> pretty a
  pretty CExprCall    { _fname = "return", _fparams = [a] } = group $ "return" <+> pretty a
  pretty CExprCall    { _fname = "Nat.eqb", _fparams = [a, b] } = (pretty a) <+> "==" <+> (pretty b)
  pretty CExprCall    { .. } = group $ pretty (toCName _fname) <> "(" <> breakcommatize _fparams <> ")"
  pretty CExprVar     { .. } = pretty _var
  pretty CExprStr     { .. } = "String(\"" <> pretty _str <> "\")"
  pretty CExprNat     { .. } = "(Nat)" <> pretty _nat
  pretty CExprBool    { .. } = pretty _bool
  pretty CExprList    { .. } = "List<T>{" <> commatize _elems <> "}"
  pretty CExprCtor    { .. } = commatize _cargs
  pretty CExprTuple   { .. } = group ("mkTuple" <> (parens . breakcommatize $ _items))
  pretty CExprStmt    { _sname = "_", .. } = pretty _sbody
  pretty CExprStmt    { .. } = pretty _stype <+> pretty _sname <+> "=" <+> pretty _sbody
  pretty CExprSeq     { .. } = pretty _left <> ";" <> line <> pretty _right
  pretty CExprWild     {}    = "Otherwise"
