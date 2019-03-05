{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Parser.Decl where
import Parser.Expr
import Data.Aeson
import Data.Text (Text)
import Data.HashMap.Strict

-- Declarations
data Declaration =
    IndDecl { name :: Text, iargs :: [Text], constructors :: [Ind] }
  | TypeDecl { name :: Text, targs :: [Text], tval :: Type }
  | FixDecl { fixlist :: [Fix] }
  | TermDecl { name :: Text, typ :: Type, val :: Expr }
    deriving (Show, Eq)

instance FromJSON Declaration where
  parseJSON (Object v) =
      case v ! "what" of
        "decl:ind"      -> IndDecl  <$> v .:  "name"
                                    <*> v .:? "argnames"     .!= []
                                    <*> v .:? "constructors" .!= []
        "decl:type"     -> TypeDecl <$> v .:  "name"
                                    <*> v .:? "argnames"     .!= []
                                    <*> v .:  "value"
        "decl:fixgroup" -> FixDecl  <$> v .:? "fixlist"      .!= []
        "decl:term"     -> TermDecl <$> v .:  "name"
                                    <*> v .:  "type"
                                    <*> v .:  "value"
        _               -> fail $ "Unknown declaration type: " ++ show v
  parseJSON _ = fail $ "Unknown declaration JSON representation"
