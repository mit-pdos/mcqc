{-# LANGUAGE OverloadedStrings #-}
module Codegen.Rewrite where
import Common.Utils
import Common.Config
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Char as C

-- String rewriting for low-level translation of Gallina base types to C++ base types
toCTBase :: Text -> Text
toCTBase "Datatypes.prod" = "tuple"
toCTBase "Datatypes.unit" = "void"
toCTBase s = rewrite s
    where prefixes   = "String.":"Datatypes.":"coq_":map mkmodule libs
          mkmodule m = T.concat ["M", T.toTitle m, ".", T.toTitle m, "."]
          rewrite    = foldr (.) id $ map (\m -> safeStripPrefix m) prefixes

-- String rewriting for low-level translation of Gallina function names to C++ names
toCName :: Text -> Text
toCName "Datatypes.Some" = "some"
toCName "Datatypes.None" = "none"
toCName s
    | T.isInfixOf "'" s  = error "Symbol not allowed in C names: '"
    | T.isInfixOf "\"" s = error "Symbol not allowed in C names: \""
    | otherwise = rewrite s
    where prefixes = "Datatypes.":"Coq_": map mkmodule libs
          mkmodule m = T.concat ["M", T.toTitle m, ".", T.toTitle m, "."]
          rewrite  = foldr (.) id $ map (\m -> safeStripPrefix m) prefixes
