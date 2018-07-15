{-# LANGUAGE TemplateHaskell, DeriveAnyClass, DeriveGeneric, RecordWildCards, OverloadedStrings  #-}
module Codegen.Expr where
import GHC.Generics
import Codegen.Defs
import Codegen.Utils
import Parser.Pattern
import Parser.Decl
import Parser.Expr
import Control.Lens
import Data.Aeson
import Data.Maybe
import Data.Text (Text)

-- C++ Expressions
data CExpr =
            CExprLambda { _largs :: [CDef], _lbody :: CExpr }
          | CExprCase { _cexpr :: CExpr, _cases :: [CExpr] }
          | CExprMatch { _mpat :: CExpr, _mbody :: CExpr }    -- Matched to Case
          | CExprCall { _fname :: Text, _fparams :: [CExpr] } -- Use this for function calls and constructors
          -- Patterns
          | CExprCtor { _cname :: Text, _cargs :: [CDef] }
          | CExprTuple { _items :: [CExpr] }
          | CExprWild {}                                      -- Wildcard pattern, matches everything
          -- Reduced forms
          -- TODO: Add optional and nested types
          | CExprStr { _str :: Text }
          | CExprNat { _nat :: Int }
          | CExprListNat { _nats :: [Int] }
          | CExprListStr { _strs :: [Text] }
    deriving (Eq, Generic, ToJSON)

-- Generate lenses
makeLenses ''CExpr

-- Get type of name
-- TODO: Complete me
--getType :: CExpr -> Text -> Maybe Text
--getType CExprLambda { .. } name = case filter (\def -> if cname def == name then True else False) (map getTypeDef largs) of
--    [] -> Nothing
--    [a] -> Just a
--    [a:as] -> error $ "getType: One or more lambda args with the same name " ++ name
--getType CExprCtor   { .. } name = case filter (\def -> if cname def == name then True else False) (map getTypeDef cargs) of
--    [] -> Nothing
--    [a] -> Just a
--    [a:as] -> error $ "getType: One or more constructor args with the same name " ++ name

-- Expression rewritting
toCExpr :: Expr -> CExpr
toCExpr ExprLambda      { .. } = CExprStr "<PLACEHOLDER FOR LAMBDA>" -- CExprLambda (getCDefExtrap argtypes argnames) (toCExpr body)
toCExpr ExprCase        { .. } = CExprCase (toCExpr expr) (map caseCExpr cases)
toCExpr ExprConstructor { .. } = CExprCall name (map toCExpr args)
toCExpr ExprApply       { func = ExprGlobal { .. }, .. } = CExprCall name (map toCExpr args)
toCExpr ExprApply       { func = ExprRel    { .. }, .. } = CExprCall name (map toCExpr args)
toCExpr ExprRel         { .. } = CExprStr name
toCExpr ExprGlobal      { .. } = CExprStr name

-- Pattern rewritting
toCPattern :: Pattern -> CExpr
toCPattern PatCtor      { .. } = CExprCtor name (map (\x -> CDef x Nothing) argnames) -- Make untyped Ctor, will be guilded with type later
toCPattern PatTuple     { .. } = CExprTuple (map toCPattern items)
toCPattern PatRel       { .. } = CExprStr name
toCPattern PatWild      {}     = CExprWild

-- Case rewrittting
caseCExpr :: Case -> CExpr
caseCExpr Case          { .. } = CExprMatch (toCPattern pat) (toCExpr body)

