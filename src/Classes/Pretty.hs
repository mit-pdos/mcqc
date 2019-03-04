{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Classes.Pretty where
import CIR.Expr
import CIR.Decl
import Classes.Typeful
import Common.Pretty
import Data.Text.Prettyprint.Doc
import qualified Data.Text as T
import qualified Data.List as L

-- Pretty printer
instance Pretty CDef where
    pretty CDef { .. } = pretty _ty <+> pretty _nm

instance Pretty CType where
  pretty CTFunc  { .. } = group $ pretty _fret <> (parens . commatize $ map pretty _fins)
  pretty CTExpr  { .. } = pretty _tbase <> "<" <> commatize (map pretty _tins) <> ">"
  pretty CTVar   { .. } = pretty _vname <> "<" <> commatize (map pretty _vargs) <> ">"
  pretty CTBase  { .. } = pretty _base
  -- Use template letters starting at T as is custom in C++
  pretty CTFree  { .. } = pretty $ stringsFromTo 'T' 'Z' !! (_idx - 1)
  pretty CTAuto  {}     = "auto" :: Doc ann
  pretty CTPtr   { .. } = "std::shared_ptr<" <> pretty _inner <> ">"

instance Pretty CExpr where
  pretty CExprLambda { _lbody = s@CExprSeq { .. }, .. } =
                            group $ "[=](" <> (commatize . map pretty $ _lds) <> ") {"
                            <> line
                            <> tab (pretty s)
                            <> line
                            <> "}"
  pretty CExprLambda { .. } =
                            group $ "[=](" <> (commatize . map pretty $ _lds) <> ") {"
                            <+> "return" <+> pretty _lbody <> ";"
                            <+> "}"
  pretty CExprCall   { _cd = CDef { _nm = "return" }, _cparams = [a] } = "return" <+> pretty a <> ";"
  pretty CExprCall   { _cd = CDef { _nm = "eqb" }, _cparams = [a, b] } = pretty a <+> "==" <+> pretty b
  pretty CExprCall   { _cd = CDef { _nm = "ltb" }, _cparams = [a, b] } = pretty a <+> "<"  <+> pretty b
  pretty CExprCall   { _cd = CDef { _nm = "leb" }, _cparams = [a, b] } = pretty a <+> "<=" <+> pretty b
  pretty CExprCall   { _cd = CDef { _nm = "match" }, .. } = "match" <> (parens . breakcommatize $ _cparams)
  pretty CExprCall   { _cd = CDef { _ty = CTAuto, .. }, .. } = pretty _nm <> (parens . commatize $ map pretty _cparams)
  pretty CExprCall   { _cd = CDef { .. }, .. } =
    pretty _nm <> "<" <> pretty _ty <> ">" <> (parens . commatize $ map pretty _cparams)
  pretty CExprVar    { .. } = pretty _var
  pretty CExprStr    { .. } = "string(\"" <> pretty _str <> "\")"
  pretty CExprNat    { .. } = "(nat)" <> pretty _nat
  pretty CExprBool   { .. } = pretty . T.toLower . T.pack . show $ _bool
  pretty CExprPair   { .. } = "std::make_pair" <> (parens $ pretty _fst <> "," <+> pretty _snd)
  pretty CExprSeq    { .. } = pretty _left <> ";" <> line <> pretty _right
  pretty CExprStmt   { _sd = CDef { _nm = "_", .. }, .. } = pretty _sbody
  pretty CExprStmt   { _sd = CDef { .. }, .. } = pretty _ty <+> pretty _nm <+> "=" <+> pretty _sbody

-- Pretty print a declaration
instance Pretty CDecl where
  pretty CDFunc { _fd = CDef { .. }, .. } =
          mkTemplateLine (_ty:map gettype _fargs)
          <> pretty _ty <+> pretty _nm <> "(" <> (commatize . raiseFuncs nfreevars $ _fargs) <> ") {"
          <> line <> (tab . pretty $ _fbody)
          <> line <> "}"
          <> line
          where nfreevars = maximum . map getMaxVaridx $ (_ty:map gettype _fargs)
                raiseFuncs n (CDef { _ty = CTFunc { .. }, .. }:ts) = pretty (CDef _nm $ CTFree (n+1)):raiseFuncs (n+1) ts
                raiseFuncs n (h:ts) = pretty h:raiseFuncs n ts
                raiseFuncs _ [] = []
  pretty CDType { _td = CDef { .. }, .. } =
          mkTemplateLine [_ty]
          <> "using" <+> pretty _nm <+> "=" <+> pretty _ty <> ";" <> line
  pretty CDStruct { _fields = [], .. } =
          mkTemplateLine [CTFree i | i <- [1.._nfree]]
          <> "struct" <+> pretty _sn <+> "{};"
  pretty CDStruct { .. } =
          mkTemplateLine (map gettype _fields)
          <> "struct" <+> pretty _sn <+> "{"
          <> line <> (tab . vcat . map (\x -> pretty x <> ";") $ _fields)
          <> line <> (tab $ pretty _sn <> "(" <> (commatize . map pretty $ _fields) <> ") {")
          <> line <> (tab . tab . vcat . map (\x -> "this->" <> toNm x <+> "=" <+> toNm x <> ";") $ _fields)
          <> line <> (tab $ "};")
          <> line <> "};" <> line
    where toNm = pretty . _nm
  pretty CDEmpty {} = mempty
  pretty CDSeq { _right = CDEmpty, .. } = pretty _left
  pretty CDSeq { _left  = CDEmpty, .. } = pretty _right
  pretty CDSeq { .. } = vcat [pretty _left, pretty _right]

-- Pretty print the template line
mkTemplateLine :: [CType] -> Doc ann
mkTemplateLine argsT
    | length (prettytempl nfreevars argsT) > 0 = "template<" <> (commatize . L.nub $ prettytempl nfreevars argsT) <> ">" <> line
    | otherwise = mempty
    where nfreevars = maximum . map getMaxVaridx $ argsT
          mktemplate n = pretty $ stringsFromTo 'T' 'Z' !! n
          isFuncRet n CTFunc { .. } | _fret == CTFree n = True
          isFuncRet _ _ = False
          prettytempl n (CTFunc { _fret = CTFree { .. }, .. }:ts) =
            prettytempl (n+1) (_fins ++ ts) ++
            ["class" <+> mktemplate n,
             "class" <+> mktemplate (_idx-1) <+> "= std::invoke_result_t<"
                <> mktemplate n <> ","
                <+> (commatize . map pretty $ _fins) <> ">"]
          prettytempl n (CTFunc { .. }:ts) = "class" <+> mktemplate n : prettytempl (n+1) (_fins ++ ts)
          prettytempl n (CTFree { .. }:ts)
              | isFuncRet _idx `L.any` ts = prettytempl n ts
              | otherwise = "class" <+> mktemplate (_idx-1) : prettytempl n ts
          prettytempl n (CTExpr { .. }:ts) = prettytempl n (_tins ++ ts)
          prettytempl n (CTPtr  { .. }:ts) = prettytempl n (_inner:ts)
          prettytempl n (_:ts) = prettytempl n ts
          prettytempl _ [] = []
