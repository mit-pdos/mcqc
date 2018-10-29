{-# LANGUAGE  OverloadedStrings, RecordWildCards  #-}
module PrettyPrinter.Decl where
import Common.Utils
import Types.Flatten
import CIR.Decl
import CIR.Expr
import PrettyPrinter.Expr()
import PrettyPrinter.Ind
import Data.Text.Prettyprint.Doc
import qualified Data.Text as T
import qualified Data.List as L

instance Pretty CDecl where
  pretty CDFunc  { _ftype = CTFunc { .. }, .. } = templateline
          <> line <> pretty _fret <+> pretty _fn <> "(" <> commatize typedargs <> ") {"
          <> line <> (tab . pretty) _fbody
          <> line <> "}"
    where typedargs = [ pretty (fst t) <+> pretty (snd t) | t <- zip _fins _fargs ]
          templates = [ "class" <+> pretty a | a <- L.nub $ getTemplates _fret ++ concatMap getTemplates _fins]
          templateline = if null templates then mempty else "template<" <> commatize templates <> ">"
  pretty CDType  { .. } = "using" <+> pretty _tname <+> "=" <+> pretty _tval <> ";"
  pretty i@CDInd { .. } = vsep . map pretty . elaborate $ i
  pretty CDEmpty {} = mempty
  pretty e = error $ "Unhandled declaration " ++ show e
