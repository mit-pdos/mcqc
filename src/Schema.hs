{-# LANGUAGE TemplateHaskell #-}
module Schema where
import Control.Applicative
import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import Data.ByteString.Lazy.Char8 (ByteString)

data Module = Module { what :: Text, name :: Text, need_magic :: Bool, need_dummy :: Bool,
                       used_modules :: [Text], declarations :: [Declaration] } deriving (Show, Eq)

data Declaration = Declaration { what :: Text, name :: Text, argnames :: [Text], constructors :: [Constructor] } deriving (Show, Eq)

data Constructor = Constructor { name :: Text, argtypes :: [Argtype] } deriving (Show, Eq)

data Argtype = Argtype { what :: Text, name :: Text, args :: [Args] } deriving (Show, Eq)

data Args = Args { what :: Text, name :: Text } deriving (Show, Eq)

-- $(deriveJSON defaultOptions ''Module)
-- $(deriveJSON defaultOptions ''Declaration)
-- $(deriveJSON defaultOptions ''Constructor)
-- $(deriveJSON defaultOptions ''Argtype)
-- $(deriveJSON defaultOptions ''Args)

parseModule :: ByteString -> Maybe Value
parseModule buffer = decode buffer :: Maybe Value

