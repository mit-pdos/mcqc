{-# LANGUAGE OverloadedStrings #-}
module Codegen.Utils where
import Data.Monoid (mempty)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc

maybeParens :: Doc ann -> Doc ann
maybeParens x = if (show x) == "" then mempty else parens x

mkFuncSig :: Text -> [Doc ann] -> Doc ann
mkFuncSig n args = pretty n <> (maybeParens $ concatWith (\x y -> x <> "," <+> y) args)

tab :: Doc ann -> Doc ann
tab d = indent 2 d

