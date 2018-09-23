{-# LANGUAGE DuplicateRecordFields, DeriveGeneric, DeriveAnyClass  #-}
module Parser.Mod where
import GHC.Generics
import Parser.Decl
import Data.Text
import Data.Aeson

---- For casting JSON to Module
data Module = Module { name :: Text, need_magic :: Bool, need_dummy :: Bool,
                       used_modules :: Maybe [Text], declarations :: [Declaration] }
    deriving (Show, Eq, Generic, FromJSON)

