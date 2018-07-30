{-# LANGUAGE  OverloadedStrings, RecordWildCards  #-}
module PrettyPrinter.Func where
import Codegen.Func
import Codegen.Utils
import Codegen.Defs
import PrettyPrinter.Expr
import PrettyPrinter.Defs
import Sema.Pipeline
import Control.Lens
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc

prettyTempls :: [Text] -> Doc ann
prettyTempls [] = mempty
prettyTempls tt = "template<" <> commatize (map ("typename " `T.append`) tt) <> ">"
                  <> line

instance Pretty CFunc where
  pretty CFuncFix { _templtypes = tt, .. } = prettyTempls tt
                                           <> pretty _ftype <+> mkFuncSig _fname _fargs
                                           <> vcat ["{", tab mainbody, "}"]
                                           <> line
    where mainbody = "return" <+> (pretty _fbody) <> ";"
  pretty CFuncImp { _templtypes = tt, .. } = prettyTempls tt
                                           <> pretty _ftype <+> mkFuncSig _fname _fargs <+> "{"
                                           <> line
                                           <> (tab . pretty) _fstmt
                                           <> ";" <> line
                                           <> "}"
  pretty CFuncEmpty { }                    = mempty
