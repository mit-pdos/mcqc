{-# LANGUAGE DuplicateRecordFields, OverloadedStrings  #-}
module Codegen.File where
import Parser.Mod
import Codegen.Func
import Codegen.Decl

data CFile = CFile { includes :: [String], filename :: String, funcs :: [CFunc] }
    deriving (Eq)

instance Show CFile where
  show f = unlines $ (includes f) ++ ["\n"] ++ (map show (funcs f))


toCFile :: Module -> CFile
toCFile Module { name = n, used_modules = Nothing, declarations = decls } = CFile ["#include<iostream>"] (n ++ ".cpp") cfuncs
    where cfuncs = map toCDecl decls
toCFile Module { name = n, used_modules = Just ml, declarations = decls } = CFile [] (n ++ ".cpp") cfuncs
    where cfuncs = map toCDecl decls

