{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Common.Utils where
import CIR.Expr
import Codegen.Rewrite
import qualified Data.Text as T
import qualified Data.Char as C
import Data.Text (Text)
import Data.Word (Word8)
import Debug.Trace

-- Word to Character cast
w2c :: Word8 -> Char
w2c = C.chr . fromIntegral

-- Define a succ for Texts
next :: Text -> Text
next = T.pack . reverse . incrementText . reverse . T.unpack
    where incrementText []          = ['a']
          incrementText ('z':xs)    = 'a' : incrementText xs
          incrementText (x:xs)      = succ x : xs

-- Make an untyped definition
mkdef :: Text -> CDef
mkdef nm = CDef (toCName nm) CTAuto

-- Print warning
warn :: String -> a -> a
warn s = trace ("Warning: " ++ s)

-- Zip names and types to fields
zipf :: [Text] -> [CType] -> [CDef]
zipf = zipWith (\a b -> CDef a b)

-- Give an ord of names to types, useful for making constructors
givenm :: Char -> [CType] -> [CDef]
givenm c = zipf [T.pack [i] | i <- [c..]]

