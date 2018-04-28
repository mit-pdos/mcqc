{-# LANGUAGE OverloadedStrings #-}
module Codegen.Rewrite (toCType, toCName) where

toCType :: String -> String
toCType "Datatypes.nat" = "unsigned int"
toCType "Datatypes.bool" = "bool"
toCType s = s

toCName :: String -> String
toCName "Datatypes.O" = "0"
toCName "Datatypes.S" = "1 + "
toCName "Datatypes.True" = "true"
toCName "Datatypes.False" = "false"
toCName s = s

