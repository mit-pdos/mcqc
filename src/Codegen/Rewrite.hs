{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Codegen.Rewrite where
import Codegen.Expr
import Codegen.Defs
import Control.Lens
import Data.Text

toCType :: Text -> Text
toCType "Datatypes.nat" = "Nat"
toCType "Datatypes.bool" = "bool"
toCType s = s

toCName :: Text -> Text
toCName "Datatypes.True" = "true"
toCName "Datatypes.False" = "false"
toCName "Nat.add" = "add"
toCName "Nat.sub" = "sub"
toCName "Nat.mul" = "mul"
toCName "Nat.div" = "div"
toCName s = s

-- Apply toCName to a CExpr
translateCNames :: CExpr -> CExpr
translateCNames = -- single step lenses
                  over fname toCName
                  . over str toCName
                  . over (strs . traverse) toCName
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

