{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DuplicateRecordFields, OverloadedStrings  #-}
module Codegen.Expr where
import GHC.Generics hiding (Constructor)
import Data.Aeson
import Data.HashMap.Strict

-- Constuctor
data Constructor = Constructor { what :: Maybe String, name :: String, argtypes :: Maybe [Typ], argnames :: Maybe [String] }
    deriving (Show, Eq, Generic, FromJSON)

-- Argtypes
data Argtype = Argtype { what :: String, name :: String, args :: Maybe [Expr] }
    deriving (Show, Eq, Generic, FromJSON)

-- Cases
data Case = Case { what :: String, pat :: [Constructor], body :: [Expr]}
    deriving (Show, Eq, Generic, FromJSON)

-- Types TODO: Varidx
data Typ =
    TypArrow { left :: Typ, right :: Typ }
    | TypVar { name :: String, args :: Maybe [Expr]}
    | TypGlob { name :: String }
    deriving (Show, Eq)

instance FromJSON Typ where
  parseJSON (Object v) =
      case (v ! "what") of
        "type:arrow" ->  TypArrow <$> v .:  "left"
                                <*> v .:  "right"
        "type:var" ->  TypVar   <$> v .:  "name"
                                <*> v .:? "args"
        "type:glob" -> TypGlob  <$> v .:  "name"
        _ -> fail ("unknown kind: " ++ (show v))

-- Expressions
data Expr = ExprLambda { argnames :: Maybe [String], body :: [Expr] }
          | ExprCase { expr :: Expr, cases :: [Case] }
          | ExprConstructor { name :: String, args :: [Expr] }
    deriving (Show, Eq)

instance FromJSON Expr where
  parseJSON (Object v) =
      case (v ! "what") of
        "expr:lambda"      -> ExprLambda      <$> v .:? "argnames"
                                              <*> v .: "body"
        "expr:case"        -> ExprCase        <$> v .: "expr"
                                              <*> v .: "cases"
        "expr:constructor" -> ExprConstructor <$> v .: "name"
                                              <*> v .: "args"
        _               -> fail ("Unknown declaration type: " ++ (show v))


