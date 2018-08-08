{-# LANGUAGE OverloadedStrings #-}
module Codegen.Config where
import Data.Text (Text)

libs :: [Text]
libs = [ "nat", "list", "proc", "string", "tuple" ]
