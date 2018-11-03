{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Codegen.Rewrite where
import Data.Text (Text)
import qualified Common.Config as Conf
import qualified Data.Text as T

-- Strip prefix, does not fail. Examples:
--     "foo" -> "foobar" -> "bar"
--     "Datatypes" -> "Datatypes.foo" -> "foo"
--     "Datatypes" -> "Datatypes..foo" -> ".foo"
--     "bar" -> "foobar" -> "foobar"
safeStripPrefix :: Text -> Text -> Text
safeStripPrefix pre s = case T.stripPrefix (T.append pre ".") s of
    (Just stripped) -> stripped
    Nothing -> case T.stripPrefix pre s of
        (Just stripped) -> stripped
        Nothing -> s

-- String rewriting for low-level translation of Gallina base types to C++ base types
toCTBase :: Text -> Text
toCTBase "Datatypes.prod" = "tuple"
toCTBase "Datatypes.unit" = "void"
toCTBase "Ascii.ascii" = "char"
toCTBase s = rewrite s
    where prefixes   = "String":"Datatypes":"coq_":map mkmodule Conf.libs
          mkmodule m = T.concat ["M", T.toTitle m, ".", T.toTitle m, "."]
          rewrite    = foldr (.) id $ map (\m -> safeStripPrefix m) prefixes

-- String rewriting for low-level translation of Gallina function names to C++ names
toCName :: Text -> Text
toCName s
    | T.isInfixOf "'" s ||
      T.isInfixOf "\"" s = unquote s
    | otherwise = rewrite s
    where prefixes = "Datatypes":"Coq_":map mkmodule Conf.libs ++ map T.toTitle Conf.libs
          -- "lib" -> "MLib.Lib" for coq typeclasses
          mkmodule m = T.concat ["M", T.toTitle m, ".", T.toTitle m, "."]
          -- top-level string rewriting, creates a pipeline of Text->Text composed functions
          rewrite  = foldr (.) id $ map (\m -> safeStripPrefix m) prefixes
          -- "foo'" -> "fooM"
          unquote = T.map (\c -> if c == '\"' || c == '\'' then 'M' else c)

