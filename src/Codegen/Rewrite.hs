{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Codegen.Rewrite where
import Codegen.Expr
import Codegen.Defs
import Common.Utils
import Control.Lens
import Data.Text (Text)
import Data.Text as T

toCType :: Text -> Text
toCType "Datatypes.nat" = "Nat"
toCType "Datatypes.bool" = "bool"
toCType "Datatypes.list" = "List<T>"
toCType "String.string" = "String"
toCType "coq_Proc" = "proc"
toCType "coq_Bind" = "proc"
toCType "coq_Ret"  = "proc"
toCType "coq_Fd"   = "Nat"
toCType s = s

-- String rewriting for low-level translation of Gallina names to C++ names
toCName :: Text -> Text
toCName "Datatypes.Coq_nil" = "List<T>()"
toCName "Nat.modulo" = "mod"
toCName "Datatypes.Some" = "some"
toCName "Datatypes.None" = "none"
toCName "Coq_ret" = "Coq_ret"
toCName "Coq_bind" = "Coq_bind"
toCName s
    | T.isPrefixOf "Nat" s = safeStripPrefix "Nat" s
    | T.isPrefixOf "String" s = safeStripPrefix "String" s
    | T.isPrefixOf "Coq_" s = safeStripPrefix "Coq_" s
    | T.isPrefixOf "Datatypes" s = safeStripPrefix "Coq_" $ safeStripPrefix "Datatypes" s
    | T.isInfixOf "'" s = error "Symbol not allowed in C names: '"
    | T.isInfixOf "\"" s = error "Symbol not allowed in C names: \""
    | otherwise = s

-- Apply toCName to a CExpr
renames :: CExpr -> CExpr
renames = 
 -- single step lenses
 over fname toCName
 . over str toCName
 . over cname toCName
 -- nested definition lenses
 . over (cargs . traverse . name) toCName
 . over (largs . traverse . name) toCName
 -- recursive lenses
 . over lbody renames
 . over (items . traverse) renames
 . over (fparams . traverse) renames
 . over (items . traverse) renames
 . over (elems . traverse) renames

