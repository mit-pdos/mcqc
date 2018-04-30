{-# LANGUAGE OverloadedStrings #-}
module Codegen.Rewrite where
import Data.Text

toCType :: Text -> Text
toCType "Datatypes.nat" = "unsigned int"
toCType "Datatypes.bool" = "bool"
toCType s = s

toInfix :: Text -> Text
toInfix "Nat.add" = "+"
toInfix "Nat.mult" = "*"
toInfix "Nat.sub" = "-"
toInfix "Nat.div" = "/"
toInfix s = s

toCNamePat :: Text -> Text
toCNamePat "Datatypes.O" = "C<O>()"
toCNamePat "Datatypes.S" = "C<S>"
toCNamePat "Datatypes.True" = "true"
toCNamePat "Datatypes.False" = "false"
toCNamePat s = s

toCName :: Text -> Text
toCName "Datatypes.O" = "new O()"
toCName "Datatypes.S" = "new S"
toCName "Datatypes.True" = "true"
toCName "Datatypes.False" = "false"
toCName s = s

