{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Parser.Expr where
import GHC.Generics hiding (Constructor)
import Parser.Pattern
import Data.Aeson
import Data.Text
import qualified Data.HashMap.Strict as M

-- Cases
data Case = Case { pat :: Pattern, body :: Expr}
    deriving (Show, Eq, Generic, FromJSON)

data Typ =
    TypArrow { left :: Typ, right :: Typ }
    | TypVar { name :: Text, args :: [Expr] }
    | TypGlob { name :: Text, targs :: [Typ] }
    | TypVaridx { idx :: Int }
    | TypUnknown {}
    | TypDummy {}
    deriving (Show, Eq)

-- Fixpoint list items
data Fix = FixD { oname :: Maybe Text, ftyp :: Typ, value :: Expr }
           | FixE { name :: Text, body :: Expr }
    deriving (Show, Eq)

-- Expressions
data Expr = ExprLambda { argnames :: [Text], body :: Expr }
          | ExprCase { expr :: Expr, cases :: [Case] }
          | IndConstructor { name :: Text, argtypes :: [Typ] }
          | ExprConstructor { name :: Text, args :: [Expr] }
          | ExprApply { func :: Expr , args :: [Expr]}
          | ExprFix { funcs :: [Fix] }
          | ExprLet { name :: Text, nameval :: Expr, body :: Expr }
          | ExprCoerce { value :: Expr }
          | ExprRel { name :: Text }
          | ExprGlobal { name :: Text }
          | ExprDummy {}
    deriving (Show, Eq)

instance FromJSON Fix where
  parseJSON (Object v) =
      case M.lookup "what" v of
        Just "fixgroup:item" -> FixD <$> v .:? "name"
                                     <*> v .:  "type"
                                     <*> v .:  "value"
        Just "fix:item"      -> FixE <$> v .: "name"
                                     <*> v .: "body"
        _                    -> fail $ "Unknown declaration type " ++ show v

instance FromJSON Typ where
  parseJSON (Object v) =
      case M.lookup "what" v of
        Just "type:arrow"       -> TypArrow  <$> v .:  "left"
                                             <*> v .:  "right"
        Just "type:var"         -> TypVar    <$> v .:  "name"
                                             <*> v .:? "args" .!= []
        Just "type:glob"        -> TypGlob   <$> v .:  "name"
                                             <*> v .:? "args" .!= []
        Just "type:varidx"      -> TypVaridx <$> v .:  "name"
        Just "type:unknown"     -> return TypUnknown {}
        Just "type:axiom"       -> return TypUnknown {}
        Just "type:dummy"       -> return TypDummy {}
        Just s                  -> fail $ "Unknown kind: " ++ show v ++ " because " ++ show s
        Nothing                 -> fail $ "No 'what' quantifier for type: " ++ show v

instance FromJSON Expr where
  parseJSON (Object v) =
      case M.lookup "what" v of
        Just "expr:lambda"      -> ExprLambda      <$> v .:? "argnames" .!= []
                                                   <*> v .:  "body"
        Just "expr:case"        -> ExprCase        <$> v .:  "expr"
                                                   <*> v .:  "cases"
        Just "expr:constructor" -> ExprConstructor <$> v .:  "name"
                                                   <*> v .:? "args"     .!= []
        Just "expr:apply"       -> ExprApply       <$> v .:  "func"
                                                   <*> v .:? "args"     .!= []
        Just "expr:coerce"      -> ExprCoerce      <$> v .:  "value"
        Just "expr:fix"         -> ExprFix         <$> v .:  "funcs"
        Just "expr:let"         -> ExprLet         <$> v .:  "name"
                                                   <*> v .:  "nameval"
                                                   <*> v .:  "body"
        Just "expr:rel"         -> ExprRel         <$> v .:  "name"
        Just "expr:global"      -> ExprGlobal      <$> v .:  "name"
        Just "expr:axiom"       -> return ExprDummy {}
        Just "expr:dummy"       -> return ExprDummy {}
        Just s                  -> fail $ "Unknown expr: " ++  show v  ++ " because " ++ show s
        Nothing                 -> IndConstructor  <$> v .:  "name"
                                                   <*> v .:? "argtypes"     .!= []
