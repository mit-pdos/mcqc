{-# LANGUAGE TemplateHaskell, DeriveGeneric, RecordWildCards, DeriveAnyClass, OverloadedStrings  #-}
module PrettyPrinter.Defs where
import Codegen.Defs
import Data.Text (Text)
import Data.Text.Prettyprint.Doc

-- Pretty print C++
instance Pretty CDef where
  pretty CDef { .. } = (pretty _typename) <+> (pretty _name)

