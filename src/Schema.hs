module Schema where
import Control.Applicative
import Data.Aeson
import Data.Text (Text)


data Module = Module { what :: Text, name :: Text, need_magic :: Bool, need_dummy :: Bool,
                       used_modules :: [Text], declarations :: [Declaration] } deriving Show

data Declaration = Declaration { what :: Text, name :: Text, argnames :: [Text], constructors :: [Constructor] } deriving Show

data Constructor = Constructor { name :: Text, argtypes :: [Argtype] } deriving Show

data Argtype = Argtype { what :: Text, name :: Text, args :: [Args] } deriving Show

data Args = Args { what :: Text, name :: Text } deriving Show

