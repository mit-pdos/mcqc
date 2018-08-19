{-# LANGUAGE  OverloadedStrings, RecordWildCards  #-}
module PrettyPrinter.Decl where
import Common.Utils
import Codegen.Decl
import Codegen.Defs
import PrettyPrinter.Expr
import PrettyPrinter.Defs
import Sema.Pipeline
import Control.Lens
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc

instance Pretty CDecl where
  pretty CFunc { .. } = prettyTempls _templtypes
                      <> line <> pretty _ftype <+> mkFuncSig _fname _fargs <+> "{"
                      <> line <> (tab . pretty) _fbody <> ";"
                      <> line <> "}"
    where prettyTempls [] = mempty
          prettyTempls tt = "template<" <> commatize (map (T.append "typename ") tt) <> ">"
  pretty CEmpty { }   = mempty
