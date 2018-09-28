{-# LANGUAGE OverloadedStrings #-}
module Common.Config where
import Data.Text (Text)

-- Libraries in include/*.hpp
libs :: [Text]
libs = [ "nat", "optional", "list", "proc", "show", "string", "tuple" ]

