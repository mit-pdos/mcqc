{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, TypeSynonymInstances, FlexibleInstances  #-}
module Codegen.Decl where
import Codegen.Func (CDef, CFunc, toCFunc)
import Parser.Decl

-- Fixpoint declaration to C Function
toCDecl :: Declaration -> CFunc
toCDecl FixDecl { fixlist = [fl] } = toCFunc fl

