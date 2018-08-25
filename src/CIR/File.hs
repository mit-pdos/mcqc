{-# LANGUAGE DeriveAnyClass, TemplateHaskell, DeriveGeneric  #-}
module CIR.File where
import GHC.Generics
import CIR.Decl
import Data.Aeson
import Control.Lens
import Data.Text (Text)

data CFile = CFile { _includes :: [Text], _decls :: [CDecl] }
    deriving (Eq, Generic, ToJSON)

makeLenses ''CFile

