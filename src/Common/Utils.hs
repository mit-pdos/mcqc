{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Common.Utils where
import qualified Data.Text as T
import qualified Data.Char as C
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Data.Word (Word8)

-- Utility function
-- Add parenteses if the argument needs them
maybeParens :: Doc ann -> Doc ann
maybeParens x = if (show x) == "" then mempty else parens x

-- Word to Character cast
w2c :: Word8 -> Char
w2c = C.chr . fromIntegral

-- Format and pretty print as a breakable comma-separated list
breakcommatize :: Pretty a => [a] -> Doc ann
breakcommatize [] = mempty
breakcommatize [a] = pretty a
breakcommatize (a:args) = softcommatize (pretty a) $
     align . tab $ concatWith softcommatize prettyargs
    where softcommatize x y = x <> "," <> softline <> y
          prettyargs = map pretty args

-- Format as an unbreakable comma-separated list
commatize :: [Doc ann] -> Doc ann
commatize [] = mempty
commatize args = concatWith (\x y -> x <> "," <+> y) args

-- Strip prefix, does not fail. Examples:
--     "foo" -> "foobar" -> "bar"
--     "Datatypes" -> "Datatypes.foo" -> "foo"
--     "Datatypes" -> "Datatypes..foo" -> ".foo"
--     "bar" -> "foobar" -> "foobar"
safeStripPrefix :: Text -> Text -> Text
safeStripPrefix pre s = case T.stripPrefix (T.append pre ".") s of
    (Just stripped) -> stripped
    (Nothing) -> case T.stripPrefix pre s of
        (Just stripped) -> stripped
        (Nothing) -> s

-- Define a succ for Texts
next :: Text -> Text
next = T.pack . reverse . incrementText . reverse . T.unpack
    where incrementText []          = ['a']
          incrementText ('z':xs)    = 'a' : incrementText xs
          incrementText (x:xs)      = succ x : xs

-- Add a tab
tab :: Doc ann -> Doc ann
tab d = indent 2 d


