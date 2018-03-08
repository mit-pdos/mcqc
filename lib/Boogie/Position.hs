{-# LANGUAGE DeriveDataTypeable #-}
-- | Utility for attaching source code positions to AST nodes
module Boogie.Position
    (Pos (..)
    ,SourcePos

    ,sourceLine
    ,sourceColumn
    ,sourceName

    ,samePos
    ,noPos
    ,attachPos
    ,gen
    ,attachPosBefore
    ,inheritPos
    ,inheritPos2

    ) where

import Control.Monad
import Data.Data
import Text.ParserCombinators.Parsec
import Text.Parsec.Pos

-- | Anything with a source position attached
data Pos a = Pos {
      position :: SourcePos,
      node :: a
    } deriving (Data, Typeable)

instance Eq a => Eq (Pos a) where
    (==) p1 p2 = node p1 == node p2

samePos (Pos p1 _) (Pos p2 _) = p1 == p2

instance Ord a => Ord (Pos a) where
    compare p1 p2 = compare (node p1) (node p2)

instance Show a => Show (Pos a) where
    show p = show (node p)

instance Functor Pos where
    fmap f (Pos s a) = Pos s (f a)

-- | Attach position to a node
attachPos :: SourcePos -> a -> Pos a
attachPos = Pos

-- | Dummy source position
noPos = (initialPos "<no file name>")

-- | Attach dummy position to a node
gen = attachPos noPos

attachPosM :: Monad m => m SourcePos -> m a -> m (Pos a)
attachPosM = liftM2 attachPos

-- | 'attachPosBefore' @p@ : parser that behaves like @p@, but also attaches the source position before the first token it parsed to the result
attachPosBefore :: Parser a -> Parser (Pos a)
attachPosBefore = attachPosM getPosition

-- | 'inheritPos' @f a@ : apply @f@ to @a@ and attach @a@'s position to the result
inheritPos :: (Pos a -> b) -> Pos a -> Pos b
inheritPos f a = attachPos (position a) (f a)

-- | 'inheritPos2' @f a b@ : apply @f@ to @a@ and @b@ and attach @a@'s position to the result
inheritPos2 :: (Pos a -> Pos b -> c) -> Pos a -> Pos b -> Pos c
inheritPos2 f a b = attachPos (position a) (f a b)
