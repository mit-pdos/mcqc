{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Codegen.Utils where
import Codegen.Defs
import Control.Lens
import qualified Data.Text as T
import Data.Text.Read
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Data.Monoid

-- Utility function
-- Add parenteses if the argument needs them
maybeParens :: Doc ann -> Doc ann
maybeParens x = if (show x) == "" then mempty else parens x

-- Format and pretty print as a comma-separated list
commatize :: Pretty a => [a] -> Doc ann
commatize args
    | null args = mempty
    | otherwise = concatWith (\x y -> x <> "," <+> y) $ map pretty args

-- Make function signature, ie: "int foo(int a, int b)"
mkFuncSig :: (Pretty a, Pretty b) => a -> [b] -> Doc ann
mkFuncSig n args = pretty n <> "(" <> commatize args <> ")"

-- Get the next string lexicographically
incrementText :: String -> String
incrementText []          = ['a']
incrementText ('z':xs)    = 'a' : incrementText xs
incrementText (x:xs)      = succ x : xs

-- Make untyped definitions into auto definitions
untypedDef :: Text -> CDef
untypedDef x = CDef x "auto"

-- If there are less named arguments that positional arguments in the type signature, extrapolate
-- and if clang gives an "Unused argument warning" then so be it
getCDefExtrap :: [Text] -> [Text] -> [CDef]
getCDefExtrap [] [] = []
getCDefExtrap [x] [y] = [CDef x y]
getCDefExtrap [x] (y:ys) = (CDef x y):(getCDefExtrap [succ x] ys)
getCDefExtrap (x:xs) [y] = (CDef x y):(getCDefExtrap xs [succ y])
getCDefExtrap (x:xs) (y:ys) = (CDef x y):(getCDefExtrap xs ys)

-- Adds the reference symbol to a CDef
addRef :: CDef -> CDef
addRef = over typename (\t -> T.append t "&")

-- Remove the reference symbol from a CDef
removeRef :: Text -> Text
removeRef = T.replace "&" ""

-- Remove the template decorator
removeTemplate :: Text -> Text
removeTemplate = T.replace "<T>" ""

-- Define a succ for Texts
instance Enum Text where
  succ = T.pack . reverse . incrementText . reverse . T.unpack

tab :: Doc ann -> Doc ann
tab d = indent 2 d

