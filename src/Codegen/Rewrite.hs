{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Codegen.Rewrite where
import Codegen.Expr
import Codegen.Defs
import Codegen.Utils
import Control.Lens
import Data.Text (Text)
import Data.Text as T

toCType :: Text -> Text
toCType "Datatypes.nat" = "Nat"
toCType "Datatypes.bool" = "bool"
toCType "Datatypes.list" = "List<T>"
toCType "String.string" = "String"
toCType "coq_Proc" = "proc"
toCType s = s

-- String rewriting for low-level translation of Gallina names to C++ names
toCName :: Text -> Text
toCName "Datatypes.Coq_nil" = "List<T>()"
toCName "Nat.modulo" = "mod"
toCName "Datatypes.Some" = "some"
toCName "Datatypes.None" = "none"
toCName s
    | T.isPrefixOf "Nat" s = safeStripPrefix "Nat" s
    | T.isPrefixOf "String" s = safeStripPrefix "String" s
    | T.isPrefixOf "Coq_" s = safeStripPrefix "Coq_" s
    | T.isPrefixOf "Datatypes" s = safeStripPrefix "Coq_" $ safeStripPrefix "Datatypes" s
    | T.isInfixOf "'" s = error "Symbol not allowed in C names: '"
    | T.isInfixOf "\"" s = error "Symbol not allowed in C names: \""
    | otherwise = s

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
                  . over (fparams . traverse) translateCNames
                  . over (items . traverse) translateCNames
                  . over (elems . traverse) translateCNames

