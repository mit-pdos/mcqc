{-# LANGUAGE OverloadedStrings #-}
module Common.Config where
import Data.Text (Text)

-- Libraries in include/*.hpp
libs :: [Text]
libs = [ "nat", "option", "list", "proc", "show", "copy", "string", "tuple" ]

-- Mutating functions that need a copy to become immutable, in list, string
mutables :: [Text]
mutables = [ "match", "app", "tail", "cons", "append" ]
