{-# LANGUAGE  OverloadedStrings, RecordWildCards  #-}
module PrettyPrinter.Func where
import Codegen.Func
import Codegen.Utils
import Codegen.Defs
import PrettyPrinter.Expr
import PrettyPrinter.Defs
import Sema.Pipeline
import Control.Lens
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc

instance Pretty CFunc where
  pretty CFunc { .. } = pretty _ftype <+> mkFuncSig _fname refArgs
                    <> vcat ["{", tab mainbody, "}"]
                    <> line
    where mainbody = "return" <+> (pretty _fbody) <> ";"
          refArgs = map pretty _fargs
