{-# LANGUAGE OverloadedStrings #-}
module Clang.Linker where
import Clang.FuncSig
import Codegen.File
import Data.Text hiding (map)
import Data.Text as T

link :: [Namespace] -> CFile -> CFile
link (ns:nss) m = CFile (((format . namespace) ns):(includes m)) (filename m) $ funcs m
    where
      format name = "#include \"" `T.append` name `T.append`  ".hpp\""

