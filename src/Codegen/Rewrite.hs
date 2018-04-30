{-# LANGUAGE OverloadedStrings #-}
module Codegen.Rewrite where
import Data.Text

toCType :: Text -> Text
toCType "Datatypes.nat" = "int"
toCType "Datatypes.bool" = "bool"
toCType s = s

toInfix :: Text -> Text
toInfix "Nat.add" = "+"
toInfix "Nat.mult" = "*"
toInfix "Nat.sub" = "-"
toInfix "Nat.div" = "/"
toInfix s = s

toCNamePat :: Text -> Text
toCNamePat "Datatypes.O" = "0"
toCNamePat "Datatypes.S" = "1+"
toCNamePat "Datatypes.True" = "true"
toCNamePat "Datatypes.False" = "false"
toCNamePat s = s

toCName :: Text -> Text
toCName "Datatypes.O" = "0"
toCName "Datatypes.S" = "1+"
toCName "Datatypes.True" = "true"
toCName "Datatypes.False" = "false"
toCName s = s

