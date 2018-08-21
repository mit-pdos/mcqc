{-# LANGUAGE OverloadedStrings #-}
module Codegen.Rewrite where
import Common.Utils
import Data.Text (Text)
import Data.Text as T

-- String rewriting for low-level translation of Gallina base types to C++ base types
toCTBase :: Text -> Text
toCTBase "Datatypes.nat" = "Nat"
toCTBase "Datatypes.bool" = "bool"
toCTBase "Datatypes.list" = "List"
toCTBase "String.string" = "String"
toCTBase "Datatypes.unit" = "void"
toCTBase s
    | T.isPrefixOf "coq_" s = safeStripPrefix "coq_" s
    | otherwise = s

-- String rewriting for low-level translation of Gallina names to C++ names
toCName :: Text -> Text
toCName "Datatypes.Coq_nil" = "List<T>()" -- TODO: Infer T
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

