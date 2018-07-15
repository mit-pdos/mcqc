{-# LANGUAGE  OverloadedStrings, RecordWildCards  #-}
module PrettyPrinter.Func where
import Codegen.Func
import Codegen.Utils
import PrettyPrinter.Expr
import PrettyPrinter.Defs
import Sema.Pipeline
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc

instance Pretty CFunc where
  pretty CFunc { .. } = pretty ftype <+> mkFuncSig fname (map pretty fargs)
                    <> vcat ["{", tab mainbody, "}"]
                    <> line
    where mainbody = "var<int>" <+> concatWith (surround ", ") (map pretty fvars) <> ";"
                      <> line
                      <> "return" <+> (pretty . semantics) fbody

