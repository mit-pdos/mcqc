{-# LANGUAGE RecordWildCards, DuplicateRecordFields, OverloadedStrings  #-}
module Codegen.Pattern where
import Parser.Pattern
import Codegen.Rewrite
import Data.Text.Prettyprint.Doc
import Data.Text (Text)

-- Patterns
data CPattern = CPatCtor { name :: Text,  argnames :: [Text] }
             | CPatTuple { items :: [CPattern] }
             | CPatRel  { name :: Text }
             | CPatWild {}
    deriving (Eq)

-- Expression rewritting
toCPattern :: Pattern -> CPattern
toCPattern PatCtor    { .. } = CPatCtor (toCName name) (map toCName argnames)
toCPattern PatTuple   { .. } = CPatTuple (map toCPattern items)
toCPattern PatRel     { .. } = CPatRel (toCName name)
toCPattern PatWild    {}     = CPatWild

instance Pretty CPattern where
  pretty CPatCtor  { .. } = "C" <> (parens . pretty $ name)
  pretty CPatTuple { .. } = "C" <> (parens . hsep $ map pretty items)
  pretty CPatRel   { .. } = "C" <> (parens . pretty $ name)
  pretty CPatWild  { } = "Otherwise"

