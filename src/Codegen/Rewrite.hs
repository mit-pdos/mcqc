{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Codegen.Rewrite where
import Common.Config
import Common.Utils
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Char as C

-- String rewriting for low-level translation of Gallina base types to C++ base types
toCTBase :: Text -> Text
toCTBase "Datatypes.prod" = "tuple"
toCTBase "Datatypes.unit" = "void"
toCTBase s = rewrite s
    where prefixes   = "String":"Datatypes":"coq_":map mkmodule libs
          mkmodule m = T.concat ["M", T.toTitle m, ".", T.toTitle m, "."]
          rewrite    = foldr (.) id $ map (\m -> safeStripPrefix m) prefixes

-- String rewriting for low-level translation of Gallina function names to C++ names
toCName :: Text -> Text
toCName s
    | T.isInfixOf "'" s ||
      T.isInfixOf "\"" s = unquote s
    | otherwise = rewrite s
    where prefixes = "Datatypes":"Coq_":map mkmodule libs ++ map T.toTitle libs
          -- "lib" -> "MLib.Lib" for coq typeclasses
          mkmodule m = T.concat ["M", T.toTitle m, ".", T.toTitle m, "."]
          -- top-level string rewriting, creates a pipeline of Text->Text composed functions
          rewrite  = foldr (.) id $ map (\m -> safeStripPrefix m) prefixes
          -- "foo'" -> "fooM"
          unquote = T.map (\c -> if c == '\"' || c == '\'' then 'M' else c)
