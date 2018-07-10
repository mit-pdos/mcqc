{-# LANGUAGE DeriveGeneric, DeriveAnyClass  #-}
module Codegen.Defs where
import GHC.Generics
import Data.Aeson
import Data.Text (Text, unpack, pack)
import Data.Text.Prettyprint.Doc

-- C typed definition, ie: "int foo"
data CDef = CDef { cname :: Text, ctype :: Text }
  deriving (Eq, Generic, ToJSON)

-- Pretty print C++ from these types
instance Pretty CDef where
  pretty d = hsep [(pretty . ctype) d, (pretty . cname) d]

-- Utility function
-- Get the next string lexicographically
incrementText :: String -> String
incrementText []          = ['a']
incrementText ('z':xs)    = 'a' : incrementText xs
incrementText (x:xs)      = succ x : xs

-- Define a succ for Texts
instance Enum Text where
  succ = pack . reverse . incrementText . reverse . unpack

-- If there are less named arguments that positional arguments in the type signature, extrapolate
-- and if clang gives an "Unused argument warning" then ok
getCDefExtrap :: [Text] -> [Text] -> [CDef]
getCDefExtrap [] [] = []
getCDefExtrap [x] [y] = [CDef x y]
getCDefExtrap [x] (y:ys) = (CDef x y):(getCDefExtrap [succ x] ys)
getCDefExtrap (x:xs) [y] = (CDef x y):(getCDefExtrap xs [succ y])
getCDefExtrap (x:xs) (y:ys) = (CDef x y):(getCDefExtrap xs ys)

