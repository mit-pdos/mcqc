{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Common.Utils where
import Classes.Typeful ()
import Classes.Pretty ()
import CIR.Expr
import Data.Text (Text)
import Data.Word (Word8)
import Debug.Trace
import qualified Data.Text     as T
import qualified Data.Char     as C
import qualified Common.Config as Conf

-- Word to Character cast
w2c :: Word8 -> Char
w2c = C.chr . fromIntegral

-- Print warning
warn :: String -> a -> a
warn s = trace ("Warning: " ++ s)

-- Zip names and types to fields
zipf :: [Text] -> [CType] -> [CDef]
zipf = zipWith (\a b -> CDef a b)

-- map across a list of CDefs
mapf :: (Text -> CType -> a) -> [CDef] -> [a]
mapf f = map (\d -> case d of (CDef { .. }) -> f _nm _ty)

-- Give an ord of names to types, useful for making constructors
givenm :: Char -> [CType] -> [CDef]
givenm c = zipf [T.pack [i] | i <- [c..]]

-- Wrap non-base types to a pointer
addPtr :: CType -> CType
addPtr t@CTBase { .. }
    | _base `elem` Conf.base = t
    | otherwise = CTPtr t
addPtr t@CTExpr { .. }
    | _tbase `elem` Conf.base = t
    | otherwise = CTPtr t
addPtr t@CTVar { .. }
    | _vname `elem` Conf.base = t
    | otherwise = CTPtr t
addPtr t = t

