{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Codegen.Rewrite where
import Codegen.Expr
import Codegen.Defs
import Control.Lens
import Data.Text

toCType :: Text -> Text
toCType "Datatypes.nat" = "Nat"
toCType "Datatypes.bool" = "bool"
toCType "Datatypes.list" = "List<T>"
toCType "String.string" = "string"
toCType s = s

toCName :: Text -> Text
toCName "Datatypes.Coq_true" = "true"
toCName "Datatypes.Coq_false" = "false"
toCName "Datatypes.Coq_nil" = "List<T>()"
toCName "Datatypes.Coq_cons" = "cons"
toCName "Nat.add" = "add"
toCName "Nat.sub" = "sub"
toCName "Nat.mul" = "mul"
toCName "Nat.div" = "div"
toCName "Nat.modulo" = "mod"
toCName "Nat.even" = "even"
toCName "Nat.odd" = "odd"
toCName "Datatypes.app" = "app"
toCName "String.append" = "append"
toCName s = s

-- Apply toCName to a CExpr
translateCNames :: CExpr -> CExpr
translateCNames = -- single step lenses
                  over fname toCName
                  . over str toCName
                  . over cname toCName
                  -- nested definition lenses
                  . over (cargs . traverse . name) toCName
                  . over (largs . traverse . name) toCName
                  -- recursive lenses
                  . over lbody translateCNames
                  . over (items . traverse) translateCNames
                  . over cexpr translateCNames
                  . over (cases . traverse) translateCNames
                  . over mpat translateCNames
                  . over mbody translateCNames
                  . over (fparams . traverse) translateCNames
                  . over (items . traverse) translateCNames
                  . over (elems . traverse) translateCNames

