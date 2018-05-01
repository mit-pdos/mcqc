{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DuplicateRecordFields, OverloadedStrings  #-}
module Parser.Expr where
import GHC.Generics hiding (Constructor)
import Parser.Pattern
import Data.Aeson
import Data.Text
import Data.HashMap.Strict
import Prelude hiding (lookup)

-- Cases
data Case = Case { pat :: Pattern, body :: Expr}
    deriving (Show, Eq, Generic, FromJSON)

-- Types TODO: Varidx
data Typ =
    TypArrow { left :: Typ, right :: Typ }
    | TypVar { name :: Text, args :: [Expr] }
    | TypGlob { name :: Text }
    deriving (Show, Eq)

instance FromJSON Typ where
  parseJSON (Object v) =
      case (lookup "what" v) of
        Just "type:arrow"       -> TypArrow  <$> v .:  "left"
                                             <*> v .:  "right"
        Just "type:var"         -> TypVar    <$> v .:  "name"
                                             <*> v .:? "args" .!= []
        Just "type:glob"        -> TypGlob   <$> v .:  "name"
        Just s                  -> fail ("unknown kind: " ++ (show v) ++ " because " ++ (show s))
        Nothing                 -> fail ("No 'what' quantifier for: " ++ (show v))

-- Expressions
data Expr = ExprLambda { argnames :: [Text], body :: Expr }
          | ExprCase { expr :: Expr, cases :: [Case] }
          | ExprConstructor { name :: Text, args :: [Expr] }
          | ExprApply { func :: Expr , args :: [Expr]}
          | ExprRel { name :: Text }
          | ExprGlobal { name :: Text }
    deriving (Show, Eq)

instance FromJSON Expr where
  parseJSON (Object v) =
      case (lookup "what" v) of
        Just "expr:lambda"      -> ExprLambda      <$> v .:? "argnames" .!= []
                                                   <*> v .:  "body"
        Just "expr:case"        -> ExprCase        <$> v .:  "expr"
                                                   <*> v .:  "cases"
        Just "expr:constructor" -> ExprConstructor <$> v .:  "name"
                                                   <*> v .:? "args"     .!= []
        Just "expr:apply"       -> ExprApply       <$> v .:  "func"
                                                   <*> v .:? "args"     .!= []
        Just "expr:rel"         -> ExprRel         <$> v .:  "name"
        Just "expr:global"      -> ExprGlobal      <$> v .:  "name"
        Just s                  -> fail ("Unknown expr: " ++ (show v) ++ " because " ++ (show s))
        Nothing                 -> fail ("No 'what' quantifier for: " ++ (show v))
