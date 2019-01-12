{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Parser.Mod where
import Parser.Decl
import Data.Aeson
import Data.Text
import Data.HashMap.Strict
import Prelude hiding (lookup)

---- For casting JSON to Module
data Module = Module { name :: Text, used_modules :: [Text], declarations :: [Declaration] }
    deriving (Show, Eq)

instance FromJSON Module where
  parseJSON (Object v) =
      case lookup "what" v of
        Just "module"        -> Module <$> v .:  "name"
                                       <*> v .:? "used_modules" .!= []
                                       <*> v .:? "declarations" .!= []
        Nothing                 -> fail $ "No 'what' quantifier for type: " ++ show v

