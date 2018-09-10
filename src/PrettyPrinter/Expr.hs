{-# LANGUAGE RecordWildCards, OverloadedStrings  #-}
module PrettyPrinter.Expr where
import CIR.Expr
import Common.Utils
import Codegen.Rewrite
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc

instance Pretty CType where
  pretty CTFunc { .. } = group $ pretty _fret <> (parens . commatize $ map pretty _fins)
  pretty CTExpr { .. } = pretty _tbase <> "<" <> commatize (map pretty _tins) <> ">"
  pretty CTVar  { .. } = "PLACEHOLDER for CTVar:"  <+> pretty _vname <> "<" <> (pretty _vargs) <> ">"
  pretty CTBase { .. } = pretty _base
  -- Use template letters starting at T as is custom in C++
  pretty CTFree { .. } = pretty $ ['T'..'Z'] !! (_idx - 1)
  pretty CTAuto {}     = "auto" :: Doc ann

instance Pretty CExpr where
  pretty CExprLambda { _lbody = CExprSeq { .. }, .. } =
                            group $ "[=](" <> commatize ["auto" <+> pretty a | a <- _largs] <> ") {"
                            <> line <> tab (pretty _left <> ";" <> line <> pretty _right) <> ";"
                            <> line <> "}"
  pretty CExprLambda { .. } =
                            group $ "[=](" <> commatize ["auto" <+> pretty a | a <- _largs] <> ") {"
                            <+> "return" <+> pretty _lbody <> ";"
                            <+> "}"
  pretty CExprCall   { _fname = "Coq_ret", _fparams = [a] } = "return" <+> pretty a
  pretty CExprCall   { _fname = "return", _fparams = [a] } = "return" <+> pretty a
  pretty CExprCall   { _fname = "eqb", _fparams = [a, b] } = (pretty a) <+> "==" <+> (pretty b)
  pretty CExprCall   { _fname = "ltb", _fparams = [a, b] } = (pretty a) <+> "<"  <+> (pretty b)
  pretty CExprCall   { _fname = "leb", _fparams = [a, b] } = (pretty a) <+> "<=" <+> (pretty b)
  pretty CExprCall   { _fname = "match", .. } = "match" <> (parens . breakcommatize $ _fparams)
  pretty CExprCall   { .. } = pretty (toCName _fname) <> (parens . commatize $ map pretty _fparams)
  pretty CExprVar    { .. } = pretty _var
  pretty CExprStr    { .. } = "String(\"" <> pretty _str <> "\")"
  pretty CExprNat    { .. } = "(Nat)" <> pretty _nat
  pretty CExprBool   { .. } = pretty . T.toLower . T.pack . show $ _bool
  pretty CExprList   { .. } = "List<T>{" <> commatize (map pretty _elems) <> "}"
  pretty CExprTuple  { .. } = "mkTuple" <> (parens . commatize $ map pretty _items)
  pretty CExprStmt   { _sname = "_", .. } = pretty _sbody
  pretty CExprStmt   { .. } = pretty _stype <+> pretty _sname <+> "=" <+> pretty _sbody
  pretty CExprSeq    { .. } = pretty _left <> ";" <> line <> pretty _right
