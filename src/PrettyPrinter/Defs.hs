{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveAnyClass  #-}
module PrettyPrinter.Defs where
import Codegen.Defs
import Data.Text (Text, unpack, pack)
import Data.Text.Prettyprint.Doc

-- Pretty print C++
instance Pretty CDef where
  pretty CDef { _name = n, _typ = Just t  } = (pretty t) <+> (pretty n)
  pretty CDef { _name = n, _typ = Nothing } = (pretty n)

