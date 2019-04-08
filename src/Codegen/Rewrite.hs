{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Codegen.Rewrite where
import Data.Text (Text)
import qualified Data.Text as T

-- String rewriting for low-level translation of Gallina base types to C++ base types
toCTBase :: Text -> Text
toCTBase "Datatypes.unit" = "void"
toCTBase "Ascii.ascii" = "char"
toCTBase s = unquote . last . T.splitOn "." $ s
    where unquote = T.replace "\"" "M" . T.replace "\'" "M"

-- String rewriting for low-level translation of Gallina function names to C++ names
toCName :: Text -> Text
toCName = unquote . unctor . last . T.splitOn "."
    where unquote = T.replace "\"" "M" . T.replace "\'" "M"
          -- Make constructors call the function, not the struct
          unctor  = T.replace "Coq_" "coq_"

-- Cannonicalize function names
cannonicalizeFn :: Text -> Text
cannonicalizeFn name =
    case T.stripPrefix "coq_" name of
        (Just cannon) -> cannon
        (Nothing) -> T.toLower name

