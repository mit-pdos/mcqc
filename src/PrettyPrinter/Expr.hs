{-# LANGUAGE RecordWildCards, OverloadedStrings  #-}
module PrettyPrinter.Expr where
import Codegen.Expr
import Common.Utils
import Codegen.Rewrite
import Data.Text.Prettyprint.Doc

instance Pretty CType where
  pretty CTFunc { .. } = group $ pretty _fret <> "(" <> commatize (map pretty _fins) <> ")"
  pretty CTExpr { .. } = pretty _tbase <> "<" <> commatize (map pretty _tins) <> ">"
  pretty CTVar  { .. } = "PLACEHOLDER for CTVar:"  <+> pretty _vname <> "<" <> (pretty _vargs) <> ">"
  pretty CTBase { .. } = pretty _base
  -- Use letters starting at T as is custom in C++
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
  pretty CExprCall   { _fname = "Coq_ret", _fparams = [a] } = group $ "return" <+> pretty a
  pretty CExprCall   { _fname = "return", _fparams = [a] } = group $ "return" <+> pretty a
  pretty CExprCall   { .. } = group $ pretty (toCName _fname) <> "(" <> breakcommatize _fparams <> ")"
  pretty CExprVar    { .. } = pretty _var
  pretty CExprStr    { .. } = "String(\"" <> pretty _str <> "\")"
  pretty CExprNat    { .. } = "(Nat)" <> pretty _nat
  pretty CExprBool   { .. } = pretty _bool
  pretty CExprList   { .. } = "List<T>{" <> commatize (map pretty _elems) <> "}"
  pretty CExprTuple  { .. } = group ("mkTuple" <> (parens . breakcommatize $ _items))
  pretty CExprStmt   { _sname = "_", .. } = pretty _sbody
  pretty CExprStmt   { .. } = pretty _stype <+> pretty _sname <+> "=" <+> pretty _sbody
  pretty CExprSeq    { .. } = pretty _left <> ";" <> line <> pretty _right
