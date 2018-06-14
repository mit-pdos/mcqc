{-# LANGUAGE OverloadedStrings #-}
module Codegen.Rewrite where
import Data.Text

toCType :: Text -> Text
toCType "Datatypes.nat" = "nat::Nat"
toCType "Datatypes.bool" = "bool"
toCType s = s

toCName :: Text -> Text
toCName "Datatypes.True" = "true"
toCName "Datatypes.False" = "false"
toCName s = s

