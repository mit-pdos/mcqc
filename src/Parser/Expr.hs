{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DuplicateRecordFields, OverloadedStrings  #-}
module Parser.Expr where
import GHC.Generics hiding (Constructor)
import Parser.Pattern
import Data.Aeson
import Data.Text
import Data.HashMap.Strict

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
      case (v ! "what") of
        "type:arrow" -> TypArrow <$> v .:  "left"
                                 <*> v .:  "right"
        "type:var"   -> TypVar   <$> v .:  "name"
                                 <*> v .:? "args" .!= []
        "type:glob"  -> TypGlob  <$> v .:  "name"
        _ -> fail ("unknown kind: " ++ (show v))

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
      case (v ! "what") of
        "expr:lambda"      -> ExprLambda      <$> v .:? "argnames" .!= []
                                              <*> v .:  "body"
        "expr:case"        -> ExprCase        <$> v .:  "expr"
                                              <*> v .:  "cases"
        "expr:constructor" -> ExprConstructor <$> v .:  "name"
                                              <*> v .:? "args"     .!= []
        "expr:apply"       -> ExprApply       <$> v .:  "func"
                                              <*> v .:? "args"     .!= []
        "expr:rel"         -> ExprRel         <$> v .:  "name"
        "expr:global"      -> ExprGlobal      <$> v .:  "name"
        s                  -> fail ("Unknown declaration type: " ++ (show v) ++ " because " ++ (show s))
