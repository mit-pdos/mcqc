{-# LANGUAGE RecordWildCards, OverloadedStrings  #-}
module PrettyPrinter.Expr where
import CIR.Expr
import Common.Utils
import Codegen.Rewrite
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc

instance Pretty CType where
  pretty CTFunc  { .. } = group $ pretty _fret <> (parens . commatize $ map pretty _fins)
  pretty CTExpr  { .. } = pretty _tbase <> "<" <> commatize (map pretty _tins) <> ">"
  pretty CTBase  { .. } = pretty _base
  -- Use template letters starting at T as is custom in C++
  pretty CTFree  { .. } = pretty $ ['T'..'Z'] !! (_idx - 1)
  pretty CTAuto  {}     = "auto" :: Doc ann
  pretty CTUndef {}     = warn ("Undefined type found") "" -- error "Undef type found in the end, internal error"

instance Pretty CExpr where
  pretty CExprLambda { _lbody = s@CExprSeq { .. }, .. } =
                            group $ "[=](" <> commatize ["auto" <+> pretty a | a <- _largs] <> ") {"
                            <> line
                            <> tab (pretty s)
                            <> line
                            <> "}"
  pretty CExprLambda { .. } =
                            group $ "[=](" <> commatize ["auto" <+> pretty a | a <- _largs] <> ") {"
                            <+> "return" <+> pretty _lbody <> ";"
                            <+> "}"
  pretty CExprCall   { _fname = "return", _fparams = [a] } = "return" <+> pretty a <> ";"
  pretty CExprCall   { _fname = "eqb", _fparams = [a, b] } = pretty a <+> "==" <+> pretty b
  pretty CExprCall   { _fname = "ltb", _fparams = [a, b] } = pretty a <+> "<"  <+> pretty b
  pretty CExprCall   { _fname = "leb", _fparams = [a, b] } = pretty a <+> "<=" <+> pretty b
  pretty CExprCall   { _fname = "match", .. } = "match" <> (parens . breakcommatize $ _fparams)
  pretty CExprCall   { .. } = pretty (toCName _fname) <> (parens . commatize $ map pretty _fparams)
  pretty CExprVar    { .. } = pretty _var
  pretty CExprStr    { .. } = "string(\"" <> pretty _str <> "\")"
  pretty CExprNat    { .. } = "(nat)" <> pretty _nat
  pretty CExprBool   { .. } = pretty . T.toLower . T.pack . show $ _bool
  pretty CExprOption { _otype = CTUndef, .. } = case _val of
                                (Just a)  -> "some(" <> pretty a <> ")"
                                (Nothing) -> warn ("type inference failed for none()") "none()"
  pretty CExprOption { .. } = case _val of
                                (Just a)  -> "some<" <> pretty _otype <> ">(" <> pretty a <> ")"
                                (Nothing) -> "none<" <> pretty _otype <> ">()"
  pretty CExprList   { _etype = CTUndef, .. } = "list {" <> commatize (map pretty _elems) <> "}"
  pretty CExprList   { .. } = "list<" <> pretty _etype  <> ">{" <> commatize (map pretty _elems) <> "}"
  pretty CExprTuple  { .. } = "mktuple" <> (parens . commatize $ map pretty _items)
  pretty s@CExprSeq  { .. } = vcat (map (\x -> pretty x <> ";") (init . seqToList $ s))
                            <> line
                            <> "return" <+> pretty (last . seqToList $ s) <> ";"
  pretty CExprStmt   { _sname = "_", .. } = pretty _sbody
  pretty CExprStmt   { .. } = pretty _stype <+> pretty _sname <+> "=" <+> pretty _sbody
