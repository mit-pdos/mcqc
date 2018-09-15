{-# LANGUAGE  OverloadedStrings, RecordWildCards  #-}
module PrettyPrinter.Decl where
import Common.Utils
import Common.Inference
import CIR.Decl
import CIR.Expr
import PrettyPrinter.Expr()
import Data.List (nub)
import Data.Text.Prettyprint.Doc

instance Pretty CDecl where
  pretty CDFunc  { _ftype = CTFunc { .. }, .. } = templateline
          <> line <> pretty _fret <+> pretty _fn <> "(" <> commatize typedargs <> ") {"
          <> line <> (tab . pretty) _fbody <> ";"
          <> line <> "}"
    where typedargs = [ pretty (fst t) <+> pretty (snd t) | t <- zip _fins _fargs ]
          argtemplates = concat $ map getTemplates _fins
          templates = [ "class" <+> pretty a | a <- nub $ getTemplates _fret ++ argtemplates ]
          templateline = if null templates then mempty else "template<" <> commatize templates <> ">"
  pretty CDType  { .. } = "using" <+> pretty _tname <+> "=" <+> pretty _tval <> ";"
  pretty CDEmpty {} = mempty
