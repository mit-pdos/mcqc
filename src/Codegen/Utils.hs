{-# LANGUAGE OverloadedStrings #-}
module Codegen.Utils where
import qualified Data.Text as T
import Data.Text.Read
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Data.Monoid

-- Add parenteses if the argument needs them
maybeParens :: Doc ann -> Doc ann
maybeParens x = if (show x) == "" then mempty else parens x

-- Make function signature, ie: "int foo(int a, int b)"
mkFuncSig :: Text -> [Doc ann] -> Doc ann
mkFuncSig n args = pretty n <> (maybeParens $ concatWith (\x y -> x <> "," <+> y) args)

-- Utility function
-- Get the next string lexicographically
incrementText :: String -> String
incrementText []          = ['a']
incrementText ('z':xs)    = 'a' : incrementText xs
incrementText (x:xs)      = succ x : xs

removeRef :: Text -> Text
removeRef = T.replace "&" ""

removeTemplate :: Text -> Text
removeTemplate = T.replace "<T>" ""

-- Define a succ for Texts
instance Enum Text where
  succ = T.pack . reverse . incrementText . reverse . T.unpack

tab :: Doc ann -> Doc ann
tab d = indent 2 d

