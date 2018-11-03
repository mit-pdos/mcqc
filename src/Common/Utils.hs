{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Common.Utils where
import CIR.Expr
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

-- Print warning
warn :: String -> a -> a
warn s = trace ("Warning: " ++ s)

-- Zip names and types to fields
zipf :: [Text] -> [CType] -> [CDef]
zipf = zipWith (\a b -> CDef a b)

-- Give an ord of names to types, useful for making constructors
givenm :: Char -> [CType] -> [CDef]
givenm c = zipf [T.pack [i] | i <- [c..]]

-- Get number of free parameters (Varidx)
getMaxVaridx :: CType -> Int
getMaxVaridx t = foldl max 0 $ getVaridxs t
    where getVaridxs CTFree { .. } = [_idx]
          getVaridxs CTFunc { .. } = getVaridxs _fret ++ concatMap getVaridxs _fins
          getVaridxs CTExpr { .. } = concatMap getVaridxs _tins
          getVaridxs CTPtr  { .. } = getVaridxs _inner
          getVaridxs _ = [0]


