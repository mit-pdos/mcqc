{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Common.Utils where
import Codegen.Defs
import Control.Lens
import qualified Data.Text as T
import qualified Data.Char as C
import Data.Text.Read
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Data.Monoid
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

-- Format and pretty print as an unbreakable comma-separated list
commatize :: Pretty a => [a] -> Doc ann
commatize [] = mempty
commatize args = concatWith (\x y -> x <> "," <+> y) prettyargs
    where prettyargs = map pretty args

-- Make function signature, ie: "int foo(int a, int b)"
mkFuncSig :: (Pretty a, Pretty b) => a -> [b] -> Doc ann
mkFuncSig n args = pretty n <> "(" <> commatize args <> ")"

-- Make untyped definitions into auto definitions
untypedDef :: Text -> CDef
untypedDef x = CDef x "auto"

-- Greedy strip prefix, safe. Examples:
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

-- If there are less named arguments that positional arguments in the type signature, extrapolate
-- and if clang gives an "Unused argument warning" then so be it
getCDefExtrap :: [Text] -> [Text] -> [CDef]
getCDefExtrap [] [] = []
getCDefExtrap [x] [y] = [CDef x y]
getCDefExtrap [x] (y:ys) = (CDef x y):(getCDefExtrap [next x] ys)
getCDefExtrap (x:xs) [y] = (CDef x y):(getCDefExtrap xs [next y])
getCDefExtrap (x:xs) (y:ys) = (CDef x y):(getCDefExtrap xs ys)

-- Adds the reference symbol to a CDef
addRef :: CDef -> CDef
addRef = over typename (\t -> case t of
                                ("Nat")   -> t -- Don't reference small types
                                otherwise -> T.append t "&")

-- Remove the reference symbol from a CDef
removeRef :: Text -> Text
removeRef = T.replace "&" ""

-- Hacky and bad in many ways
addTemplates :: [Text] -> [Text]
addTemplates incls
    | "List<T>" `elem` incls || "Optional<T>" `elem` incls = [ "T" ]
    | otherwise = []

-- Remove the template decorator
removeTemplate :: Text -> Text
removeTemplate = T.replace "<T>" ""

-- Define a succ for Texts
next :: Text -> Text
next = T.pack . reverse . incrementText . reverse . T.unpack
    where incrementText []          = ['a']
          incrementText ('z':xs)    = 'a' : incrementText xs
          incrementText (x:xs)      = succ x : xs

tab :: Doc ann -> Doc ann
tab d = indent 2 d

