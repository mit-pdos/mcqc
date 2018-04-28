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

-- Utility function
-- Get the next string lexicographically
incrementString :: String -> String
incrementString []          = ['a']
incrementString ('z':xs)    = 'a' : incrementString xs
incrementString (x:xs)      = succ x : xs

-- Define a succ for Strings
instance Enum [Char] where
  succ = reverse . incrementString . reverse

-- If there are less named arguments that positional arguments in the type signature, extrapolate
-- and if clang gives an "Unused argument warning" then ok
getCDefExtrap :: [String] -> [String] -> [CDef]
getCDefExtrap [] [] = []
getCDefExtrap [x] [y] = [CDef x y]
getCDefExtrap [x] (y:ys) = (CDef x y):(getCDefExtrap [succ x] ys)
getCDefExtrap (x:xs) [y] = (CDef x y):(getCDefExtrap xs [succ y])
getCDefExtrap (x:xs) (y:ys) = (CDef x y):(getCDefExtrap xs ys)

