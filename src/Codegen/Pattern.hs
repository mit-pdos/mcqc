{-# LANGUAGE RecordWildCards, DuplicateRecordFields, OverloadedStrings  #-}
module Codegen.Pattern where
import Parser.Pattern
import Codegen.Rewrite

-- Patterns
data CPattern = CPatCtor { name :: String,  argnames :: [String] }
             | CPatTuple { items :: [CPattern] }
             | CPatRel  { name :: String }
             | CPatWild {}
    deriving (Eq)

-- Expression rewritting
toCPattern :: Pattern -> CPattern
toCPattern PatCtor    { .. } = CPatCtor (toCName name) (map toCName argnames)
toCPattern PatTuple   { .. } = CPatTuple (map toCPattern items)
toCPattern PatRel     { .. } = CPatRel (toCName name)
toCPattern PatWild    {}     = CPatWild

instance Show CPattern where
  show CPatCtor  { .. } = "Pat Ctor: " ++ name
  show CPatTuple { .. } = "Pat Tuple: " ++ (show items)
  show CPatRel   { .. } = "Pat Rel: " ++ name
  show CPatWild  { } = "Pat Wild"

