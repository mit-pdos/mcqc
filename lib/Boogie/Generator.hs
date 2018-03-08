-- | Deterministic and non-deterministic input generators
module Boogie.Generator where

import Control.Monad.Identity hiding (join)
import System.Random

-- | Input generator
data Generator m = Generator {
  genBool :: m Bool,        -- Generate a boolean
  genInteger :: m Integer,  -- Generate an arbitrary precision integer
  genIndex :: Int -> m Int  -- Generate a natural smaller than a given bound
  }
  
-- | Always generates the same default value
defaultGenerator :: Generator Identity  
defaultGenerator = Generator {
  genBool = Identity False,
  genInteger = Identity 0,
  genIndex = Identity . const 0
}

-- | Generates all possible values once, in a predefined order
exhaustiveGenerator :: (MonadPlus m) => Maybe Integer -> Generator m
exhaustiveGenerator mBound = Generator {
  genBool = return False `mplus` return True,
  genInteger = case mBound of
    Nothing -> allIntegers
    Just b -> fromInterval $ intInterval b,
  genIndex = \n -> fromInterval $ case mBound of
    Nothing -> natInterval n
    Just b -> natInterval $ fromInteger (b `min` toInteger n)
}
  where
    allIntegers = fromList [0, -1..] `mplus` fromList [1..]
    fromInterval (a, b)
      | b < a = mzero
      | a >= 0 || b <= 0 = fromList [a..b]
      | otherwise = fromList [0, -1..a] `mplus` fromList [1..b]

-- | Generated values randomly; the same value can be generated multiple times
randomGenerator :: (MonadPlus m) => StdGen -> Maybe Integer -> Generator m
randomGenerator rGen mBound = Generator {
  genBool = fromList $ randoms rGen,
  genInteger = fromList $ case mBound of
    Nothing -> randoms rGen
    Just b -> randomRs (intInterval b) rGen,
  genIndex = \n -> fromList $ case mBound of
    Nothing -> randomRs (natInterval n) rGen
    Just b -> randomRs (natInterval $ fromInteger (b `min` toInteger n)) rGen
}

-- | 'intInterval' @n@: interval centered around 0 of size n
intInterval n = let n2 = n `div` 2 in (-n2, n - n2 - 1)

-- | 'natInterval' @n@: interval starting from 0 of size n
natInterval n = (0, n - 1)

-- | Convert a (possibly infinite) nonempty list into a stream      
fromList :: MonadPlus m => [a] -> m a
fromList xs = foldr mplus mzero (map return xs)
