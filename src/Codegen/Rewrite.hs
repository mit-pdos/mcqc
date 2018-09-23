{-# LANGUAGE OverloadedStrings #-}
module Codegen.Rewrite where
import Common.Utils
import Data.Text (Text)
import qualified Data.Text as T

-- String rewriting for low-level translation of Gallina base types to C++ base types
toCTBase :: Text -> Text
toCTBase "MNat.Nat.nat" = "Nat"
toCTBase "Datatypes.bool" = "bool"
toCTBase "MList.List.list" = "List"
toCTBase "MString.String.string" = "String"
toCTBase "Datatypes.nat" = "Nat"
toCTBase "Datatypes.list" = "List"
toCTBase "String.string" = "String"
toCTBase "Datatypes.unit" = "void"
toCTBase s
    | T.isPrefixOf "coq_" ss = safeStripPrefix "coq_" ss
    | otherwise = ss
    where ss = last $ T.splitOn "." s

-- String rewriting for low-level translation of Gallina function names to C++ names
toCName :: Text -> Text
toCName "Datatypes.Some" = "some"
toCName "Datatypes.None" = "none"
toCName s
    | T.isPrefixOf "Nat" ss = safeStripPrefix "Nat" ss
    | T.isPrefixOf "String" ss = safeStripPrefix "String" ss
    | T.isPrefixOf "Coq_" ss = stripcoq ss
    | T.isPrefixOf "Datatypes" ss = stripcoq $ safeStripPrefix "Datatypes" ss
    | T.isInfixOf "'" ss = error "Symbol not allowed in C names: '"
    | T.isInfixOf "\"" ss = error "Symbol not allowed in C names: \""
    | otherwise = ss
    where stripcoq = safeStripPrefix "Coq_"
          ss = last $ T.splitOn "." s
