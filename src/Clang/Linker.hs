{-# LANGUAGE OverloadedStrings #-}
module Clang.Linker where
import Clang.Namespaces
import Codegen.File
import Data.Text hiding (map)
import Data.Text as T

link :: [Namespace] -> CFile -> CFile
link (ns:nss) m = CFile ((namespace ns):(includes m)) (filename m) $ funcs m

