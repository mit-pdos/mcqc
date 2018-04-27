{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, TypeSynonymInstances, RecordWildCards, FlexibleInstances  #-}
module Codegen.Defs where

-- C typed definition, ie: "int foo"
data CDef = CDef { cname :: String, ctype :: String }
  deriving (Eq)

-- Pretty print C++ from these types
instance Show CDef where
  show d = (ctype d) ++ " " ++ (cname d)

-- Overlaps with Show [a]
instance {-# OVERLAPPING #-} Show [CDef] where
  show [] = ""
  show [d] = show d
  show (d:dl) = (show d) ++ ", " ++ (show dl)

