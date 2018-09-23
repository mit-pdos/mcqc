{-# LANGUAGE DuplicateRecordFields, OverloadedStrings  #-}
module Parser.Fix where
import Parser.Expr
import Data.Text (Text)
import Data.Aeson
import Data.HashMap.Strict

-- Fixpoint list items
data Fix = Fix { name :: Maybe Text, ftyp :: Typ, value :: Expr }
    deriving (Show, Eq)

instance FromJSON Fix where
  parseJSON (Object v) =
      case v ! "what" of
        "fixgroup:item" -> Fix <$> v .:? "name"
                               <*> v .:  "type"
                               <*> v .:  "value"
        _               -> error $ "Unknown declaration type " ++ show v


