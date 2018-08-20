{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Codegen.Rewrite where
import Codegen.Expr
import Codegen.Defs
import Common.Utils
import Control.Lens
import Data.Text (Text)
import Data.Text as T

-- String rewriting for low-level translation of Gallina base types to C++ base types
toCTBase :: Text -> Text
toCTBase "Datatypes.nat" = "Nat"
toCTBase "Datatypes.bool" = "bool"
toCTBase "Datatypes.list" = "List<T>"
toCTBase "String.string" = "String"
toCTBase "coq_Proc" = "proc"
toCTBase "coq_Bind" = "proc"
toCTBase "coq_Ret"  = "proc"
toCTBase "coq_Fd"   = "Nat"
toCTBase s = s

data CType =
    CTFunc { _ftype :: CType, _fargs :: [CType] }
    | CTComp { _tname :: Text, _targs :: [CType] }
    | CTFree { _idx :: Int }
    | CTBound { _base :: Text }
    deriving (Show, Eq, Generic, ToJSON)

-- Apply toCTBase to all CType
retypes :: CType -> CType
retypes = 
 -- single step lenses
 over ftype retypes
 . over tname toCTBase
 . over base toCTBase
 -- recursive lenses
 . over (targs . traverse) retypes 
 . over (fargs . traverse) retypes
 
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

