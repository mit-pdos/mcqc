{-# LANGUAGE DuplicateRecordFields, DeriveGeneric, DeriveAnyClass, OverloadedStrings  #-}
module Parser.Mod where
import GHC.Generics
import Parser.Decl
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Aeson

---- For casting JSON to Module
data Module = Module { name :: String, need_magic :: Bool, need_dummy :: Bool,
                       used_modules :: Maybe [String], declarations :: [Declaration] }
    deriving (Show, Eq, Generic, FromJSON)

