{-# LANGUAGE DuplicateRecordFields, TemplateHaskell #-}
module Schema where
import Data.Aeson
import Data.Aeson.TH

---- For casting JSON to Module
data Module = Module { what :: String, name :: String, need_magic :: Bool, need_dummy :: Bool,
                       used_modules :: Maybe [String], declarations :: [Declaration] } deriving (Show, Eq)

data Declaration = Declaration { what :: String, name :: String, argnames :: Maybe [String], typ :: Maybe Typ, constructors :: Maybe [Constructor] } deriving (Show, Eq)
data Typ = Typ { what :: String, left :: Argtype, right :: Argtype } deriving (Show, Eq)
data Body = Body { what :: String, expr :: Arg, cases :: [Cases] } deriving (Show, Eq)
data Cases = Cases { what :: String, pat :: [Constructor], body :: Maybe Body } deriving (Show, Eq)
data Constructor = Constructor { what :: Maybe String, name :: String, argtypes :: Maybe [Argtype], argnames :: Maybe [String] } deriving (Show, Eq)
data Argtype = Argtype { what :: String, name :: String, args :: Maybe [Arg] } deriving (Show, Eq)
data Arg = Arg { what :: String, name :: Value } deriving (Show, Eq)

$(deriveJSON defaultOptions ''Module)
$(deriveJSON defaultOptions ''Declaration)
$(deriveJSON defaultOptions ''Constructor)
$(deriveJSON defaultOptions ''Typ)
$(deriveJSON defaultOptions ''Body)
$(deriveJSON defaultOptions ''Cases)
$(deriveJSON defaultOptions ''Argtype)
$(deriveJSON defaultOptions ''Arg)

