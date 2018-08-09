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
import Data.Text (Text)

-- C++ Expressions
data CExpr =
            CExprLambda { _largs :: [CDef], _lbody :: CExpr }
          | CExprCall { _fname :: Text, _fparams :: [CExpr] } -- Use this for function calls and constructors
          | CExprStmt { _stype :: Text, _sname :: Text, _sbody :: CExpr } -- C++ statament for monadic unrolling
          -- Patterns
          | CExprCtor { _cname :: Text, _cargs :: [CDef] }
          | CExprTuple { _items :: [CExpr] }
          | CExprWild {}                                      -- Wildcard pattern, matches everything
          -- Reduced forms
          -- TODO: Add optional and nested types
          | CExprVar { _var :: Text }
          | CExprStr { _str :: Text }
          | CExprNat { _nat :: Int }
          | CExprBool { _bool :: Bool }
          | CExprList { _elems :: [CExpr] }
          -- Continuation
          | CExprSeq { _left :: CExpr, _right :: CExpr }
    deriving (Eq, Generic, ToJSON, Show)

-- Generate lenses
makeLenses ''CExpr

-- get names that appear in expression
getNames :: Expr -> [Text]
getNames ExprLambda { .. } = argnames
getNames ExprRel    { .. } = [name]
getNames ExprGlobal { .. } = [name]
getNames ExprCoerce { .. } = getNames value

-- Pattern rewritting
toCDefs :: Pattern -> [CDef]
toCDefs PatCtor      { .. } = map untypedDef argnames
toCDefs PatTuple     { .. } = error "Tuple patterns not implemented yet"
toCDefs PatRel       { .. } = [untypedDef name]
toCDefs PatWild      {}     = [untypedDef "_"]

-- Expression rewritting
toCExpr :: Expr -> CExpr
toCExpr ExprLambda      { .. } = CExprLambda (map untypedDef argnames) (toCExpr body)
toCExpr ExprCase        { .. } = CExprCall "match" $ (toCExpr expr):(map mkCase cases)
    where mkCase Case   { .. } = CExprLambda (toCDefs pat) (toCExpr body)
toCExpr ExprConstructor { .. } = CExprCall name (map toCExpr args)
toCExpr ExprApply       { func = ExprGlobal { .. }, .. } = CExprCall name (map toCExpr args)
toCExpr ExprApply       { func = ExprRel    { .. }, .. } = CExprCall name (map toCExpr args)
toCExpr ExprApply       { .. } = CExprCall (head . getNames $ func) (map toCExpr args)
toCExpr ExprRel         { .. } = CExprVar name
toCExpr ExprGlobal      { .. } = CExprVar name
toCExpr ExprCoerce      { .. } = toCExpr value
toCExpr ExprDummy       {    } = CExprVar ""
toCExpr e                      = error $ "Match fell through " ++ (show e)

