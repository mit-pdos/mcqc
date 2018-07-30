{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveAnyClass  #-}
module Codegen.Defs where
import GHC.Generics
import Control.Lens
import Data.Aeson
import Data.Text (Text)

-- C typed definition
-- If it is untyped ie: _typ = Nothing, we need to extrapolate the type before pp
data CDef = CDef { _name :: Text, _typename :: Text }
  deriving (Eq, Generic, ToJSON, Show)

-- Generate lenses
makeLenses ''CDef

