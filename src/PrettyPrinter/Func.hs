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

instance Pretty CFunc where
  pretty CFunc { _templtypes = [], .. } = pretty _ftype <+> mkFuncSig _fname _fargs
                                           <> funcbody
    where funcbody = vcat ["{", tab mainbody, "}"] <> line
          mainbody = "return" <+> (pretty _fbody) <> ";"
  pretty CFunc { _templtypes = tt, .. } = "template<" <> commatize (map ("typename " `T.append`) tt) <> ">"
                                           <> line
                                           <> pretty _ftype <+> mkFuncSig _fname _fargs
                                           <> funcbody
    where funcbody = vcat ["{", tab mainbody, "}"] <> line
          mainbody = "return" <+> (pretty _fbody) <> ";"
  pretty CFuncEmpty { }      = mempty
