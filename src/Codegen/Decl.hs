{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, TypeSynonymInstances, FlexibleInstances  #-}
module Codegen.Decl where
import Codegen.Func
import Parser.Decl

-- Fixpoint declaration to C Function
toCDecl :: Declaration -> CFunc
toCDecl FixDecl { fixlist = [fl] } = toCFunc fl

