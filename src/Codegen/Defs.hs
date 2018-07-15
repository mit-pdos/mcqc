{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveAnyClass  #-}
module Codegen.Defs where
import GHC.Generics
import Codegen.Utils
import Control.Lens
import Data.Aeson
import Data.Maybe
import Data.Text (Text)

-- C typed definition
-- If it is untyped ie: _typ = Nothing, we need to extrapolate the type before pp
data CDef = CDef { _name :: Text, _typ :: Maybe Text }
  deriving (Eq, Generic, ToJSON)

-- Generate lenses
makeLenses ''CDef

-- If there are less named arguments that positional arguments in the type signature, extrapolate
-- and if clang gives an "Unused argument warning" then so be it
getCDefExtrap :: [Text] -> [Text] -> [CDef]
getCDefExtrap [] [] = []
getCDefExtrap [x] [y] = [CDef x (Just y)]
getCDefExtrap [x] (y:ys) = (CDef x (Just y)):(getCDefExtrap [succ x] ys)
getCDefExtrap (x:xs) [y] = (CDef x (Just y)):(getCDefExtrap xs [succ y])
getCDefExtrap (x:xs) (y:ys) = (CDef x (Just y)):(getCDefExtrap xs ys)

