{-# LANGUAGE DuplicateRecordFields, OverloadedStrings  #-}
module Parser.Fix where
import GHC.Generics
import Parser.Expr
import Data.Text (Text)
import Data.Aeson
import Data.HashMap.Strict

-- Fixpoint list items
data Fix = Fix { name :: Maybe Text, typ :: Typ, value :: Expr }
    deriving (Show, Eq)

instance FromJSON Fix where
  parseJSON (Object v) =
      case (v ! "what") of
        "fixgroup:item" -> Fix <$> v .:? "name"
                               <*> v .:  "type"
                               <*> v .:  "value"
        _               -> fail ("Unknown declaration type: " ++ (show v))


