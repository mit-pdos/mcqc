{-# LANGUAGE RecordWildCards, DuplicateRecordFields, DeriveGeneric, DeriveAnyClass, OverloadedStrings  #-}
module Codegen.Pattern where
import GHC.Generics
import Parser.Pattern
import Codegen.Rewrite
import Codegen.Utils
import Data.Aeson
import Data.Text.Prettyprint.Doc
import Data.Text (Text)

-- Patterns
data CPattern = CPatCtor { name :: Text,  argnames :: [Text] }
             | CPatTuple { items :: [CPattern] }
             | CPatRel  { name :: Text }
             | CPatWild {}
    deriving (Eq, Generic, ToJSON)

-- Expression rewritting
toCPattern :: Pattern -> CPattern
toCPattern PatCtor    { .. } = CPatCtor (toCName name) (map toCName argnames)
toCPattern PatTuple   { .. } = CPatTuple (map toCPattern items)
toCPattern PatRel     { .. } = CPatRel (toCName name)
toCPattern PatWild    {}     = CPatWild

instance Pretty CPattern where
  pretty CPatCtor  { .. } = mkFuncSig name $ map pretty argnames
  pretty CPatTuple { .. } = "C" <> (parens . hsep $ map pretty items)
  pretty CPatRel   { .. } = "C" <> (parens . pretty $ name)
  pretty CPatWild  { } = "Otherwise"

