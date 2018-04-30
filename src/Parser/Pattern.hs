{-# LANGUAGE DuplicateRecordFields, OverloadedStrings  #-}
module Parser.Pattern where
import GHC.Generics
import Data.Aeson
import Data.Text
import Data.HashMap.Strict

-- Patterns
data Pattern = PatCtor { name :: Text,  argnames :: [Text] }
             | PatTuple { items :: [Pattern] }
             | PatRel  { name :: Text }
             | PatWild {}
    deriving (Show, Eq)

instance FromJSON Pattern where
  parseJSON (Object v) =
      case (v ! "what") of
        "pat:constructor" -> PatCtor          <$> v .:  "name"
                                              <*> v .:? "argnames" .!= []
        "pat:tuple"       -> PatTuple         <$> v .:? "items" .!= []
        "pat:rel"         -> PatRel           <$> v .:  "name"
        "pat:wild"        -> return PatWild
        s                 -> fail ("Unknown declaration type: " ++ (show v) ++ " because " ++ (show s))
