{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DuplicateRecordFields, OverloadedStrings  #-}
module Codegen.File where
import GHC.Generics
import Parser.Mod
import Data.Aeson
import Codegen.Func
import Codegen.Decl
import Clang.Namespaces
import Data.Text (Text)
import qualified Data.Text as T

data CFile = CFile { includes :: [Text], filename :: Text, funcs :: [CFunc] }
    deriving (Eq, Generic, ToJSON)

compile :: Module -> CFile
compile Module { name = n, used_modules = Nothing, declarations = decls } = CFile [] (T.append n ".cpp") cfuncs
    where cfuncs = map toCDecl decls
compile Module { name = n, used_modules = Just ml, declarations = decls } = CFile [] (T.append n ".cpp") cfuncs -- TODO: implement linking with other coq files
    where cfuncs = map toCDecl decls


