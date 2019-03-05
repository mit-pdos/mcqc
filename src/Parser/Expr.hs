{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Parser.Expr where
import GHC.Generics hiding (Constructor)
import Data.Aeson
import Data.Text
import qualified Data.HashMap.Strict as M

-- Patterns
data Pattern = PatCtor { name :: Text,  argnames :: [Text] }
             | PatTuple { items :: [Pattern] }
             | PatRel  { name :: Text }
             | PatWild {}
    deriving (Show, Eq)

-- Cases
data Case = Case { pat :: Pattern, body :: Expr }
    deriving (Show, Eq, Generic, FromJSON)

-- Types
data Type =
    TArrow { left :: Type, right :: Type }
    | TVar { name :: Text, args :: [Expr] }
    | TGlob { name :: Text, targs :: [Type] }
    | TVaridx { idx :: Int }
    | TUnknown {}
    | TDummy {}
    deriving (Show, Eq)

-- Fixpoint list items
data Fix = Fix { name :: Maybe Text, body :: Expr, typ :: Type }
    deriving (Show, Eq)

-- Inductive constructor
data Ind = Ind { name :: Text, argtypes :: [Type] }
    deriving (Show, Eq, Generic, FromJSON)

-- Expressions
data Expr = ExprLambda { argnames :: [Text], body :: Expr }
          | ExprCase { expr :: Expr, cases :: [Case] }
          | ExprConstructor { name :: Text, args :: [Expr] }
          | ExprApply { func :: Expr , args :: [Expr] }
          | ExprFix { funcs :: [Fix] }
          | ExprLet { name :: Text, nameval :: Expr, body :: Expr }
          | ExprCoerce { value :: Expr }
          | ExprVar { name :: Text }
          | ExprDummy {}
    deriving (Show, Eq)

instance FromJSON Pattern where
  parseJSON (Object v) =
      case M.lookup "what" v of
        Just "pat:constructor" -> PatCtor          <$> v .:  "name"
                                                   <*> v .:? "argnames" .!= []
        Just "pat:tuple"       -> PatTuple         <$> v .:? "items" .!= []
        Just "pat:rel"         -> PatRel           <$> v .:  "name"
        Just "pat:wild"        -> pure PatWild
        _                      -> fail $ "Unknown pattern object: " ++ show v
  parseJSON _ = fail $ "Unknown pattern JSON representation"

instance FromJSON Fix where
  parseJSON (Object v) =
      case M.lookup "what" v of
        Just "fixgroup:item" -> Fix <$> v .:? "name"
                                    <*> v .:  "value"
                                    <*> v .:  "type"
        Just "fix:item"      -> Fix <$> v .:? "name"
                                    <*> v .:  "body"
                                    <*> pure TUnknown
        _                    -> fail $ "Unknown fixpoint type " ++ show v
  parseJSON _ = fail $ "Unknown fixpoint JSON representation"

instance FromJSON Type where
  parseJSON (Object v) =
      case M.lookup "what" v of
        Just "type:arrow"       -> TArrow  <$> v .:  "left"
                                           <*> v .:  "right"
        Just "type:var"         -> TVar    <$> v .:  "name"
                                           <*> v .:? "args" .!= []
        Just "type:glob"        -> TGlob   <$> v .:  "name"
                                           <*> v .:? "args" .!= []
        Just "type:varidx"      -> TVaridx <$> v .:  "name"
        Just "type:unknown"     -> pure TUnknown {}
        Just "type:axiom"       -> pure TUnknown {}
        Just "type:dummy"       -> pure TDummy {}
        Just s                  -> fail $ "Unknown kind: " ++ show v ++ " because " ++ show s
        Nothing                 -> fail $ "No 'what' quantifier for type: " ++ show v
  parseJSON _ = fail $ "Unknown type JSON representation"

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
        Just "expr:rel"         -> ExprVar         <$> v .:  "name"
        Just "expr:global"      -> ExprVar         <$> v .:  "name"
        Just "expr:axiom"       -> pure ExprDummy {}
        Just "expr:dummy"       -> pure ExprDummy {}
        Just s                  -> fail $ "Unknown expr: " ++  show v  ++ " because " ++ show s
        Nothing                 -> fail $ "Expression with no 'what' quanitifier is not allowed"
  parseJSON _ = fail $ "Unknown expr JSON representation"

