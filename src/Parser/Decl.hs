{-# LANGUAGE DuplicateRecordFields, OverloadedStrings  #-}
module Parser.Decl where
import GHC.Generics
import Parser.Expr
import Parser.Fix
import Data.Aeson
import Data.Text (Text)
import Data.HashMap.Strict

-- Declarations
data Declaration =
    IndDecl { name :: Text, iargs :: [Text], constructors :: [Expr] }
  | TypeDecl { name :: Text, targs :: [Text], tval :: Typ }
  | FixDecl { fixlist :: [Fix] }
  | TermDecl { name :: Text, typ :: Typ, val :: Expr }
    deriving (Show, Eq)

instance FromJSON Declaration where
  parseJSON (Object v) =
      case (v ! "what") of
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
        _               -> fail ("Unknown declaration type: " ++ (show v))


