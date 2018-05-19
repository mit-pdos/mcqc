{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module Main where
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import qualified Unbind
import qualified Datatypes
import Data.Text hiding (foldr)
import Data.Text.IO as T

instance Show (Unbind.Coq_opT ()) where
    show (Unbind.Coq_print a) = "proc::print(" ++ (show a) ++ ");"
    show (Unbind.Coq_getline) = "proc::getline"

mkList :: (Datatypes.Coq_list (Unbind.Coq_opT ())) -> [Unbind.Coq_opT ()]
mkList (Datatypes.Coq_nil) = []
mkList (Datatypes.Coq_cons a l) = a:(mkList l)

instance Show (Datatypes.Coq_list (Unbind.Coq_opT ())) where
    show cl = show . mkList $ cl

instance {-# OVERLAPPING #-} Pretty [Unbind.Coq_opT ()] where
    pretty [] = ""
    pretty (o:ol) = op <> line <> (pretty ol)
        where op = pretty $ show o

instance Pretty (Datatypes.Coq_list (Unbind.Coq_opT ())) where
    pretty cl = pretty . mkList $ cl

render :: Doc ann -> Text
render doc = (renderStrict . layoutPretty layoutOptions) cpp
    where layoutOptions = LayoutOptions { layoutPageWidth = AvailablePerLine 180 1 }
          cpp = vsep ["#include \"proc.hpp\"" :: Doc ann, line, "int main() {" :: Doc ann, indent 2 doc, "}" :: Doc ann]

main :: IO()
main = T.putStrLn $ (render . pretty) Unbind.foo_p

