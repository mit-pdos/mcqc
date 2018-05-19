{-# LANGUAGE FlexibleInstances #-}
module Main where
import Prelude
import qualified Datatypes
import qualified Unbind

instance Show (Unbind.Coq_opT ()) where
    show (Unbind.Coq_print a) = "print " ++ (show a)
    show (Unbind.Coq_getline) = "getline"

mkList :: (Datatypes.Coq_list (Unbind.Coq_opT ())) -> [Unbind.Coq_opT ()]
mkList (Datatypes.Coq_nil) = []
mkList (Datatypes.Coq_cons a l) = a:(mkList l)

instance Show (Datatypes.Coq_list (Unbind.Coq_opT ())) where
    show cl = show . mkList $ cl

main :: IO()
main = putStrLn (show Unbind.foo_p)

