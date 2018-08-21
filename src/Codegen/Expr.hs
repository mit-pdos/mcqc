{-# LANGUAGE TemplateHaskell, DeriveAnyClass, DeriveGeneric, RecordWildCards, OverloadedStrings  #-}
module Codegen.Expr where
import GHC.Generics
import Codegen.Rewrite
import Parser.Pattern
import Parser.Expr
import Control.Lens
import Data.Aeson
import Data.Text (Text)

-- C++ Types
data CType =
    CTFunc { _fret :: CType, _fins :: [CType] }
    | CTExpr { _tbase :: CType, _tins :: [CType] }
    | CTVar  { _vname :: Text, _vargs :: [CExpr] }
    | CTBase { _base :: Text }
    | CTFree { _idx :: Int }
    | CTAuto {}
    deriving (Show, Eq, Generic, ToJSON)

-- C++ Expressions
data CExpr =
          -- High level C++ expressions
            CExprLambda { _largs :: [Text], _lbody :: CExpr }
          | CExprCall { _fname :: Text, _fparams :: [CExpr] }
          -- C++ statament for monadic unrolling
          | CExprStmt { _stype :: CType, _sname :: Text, _sbody :: CExpr } 
          -- Reduced forms
          | CExprVar { _var :: Text }
          | CExprStr { _str :: Text }
          | CExprNat { _nat :: Int }
          | CExprBool { _bool :: Bool }
          | CExprList { _elems :: [CExpr] }
          | CExprTuple { _items :: [CExpr] }
          -- Continuations
          | CExprSeq { _left :: CExpr, _right :: CExpr }
    deriving (Eq, Generic, ToJSON, Show)

-- Generate lenses
makeLenses ''CExpr
makeLenses ''CType

-- Expression compiling, from Coq to C++
toCExpr :: Expr -> CExpr
toCExpr ExprLambda      { .. } = CExprLambda argnames $ toCExpr body
toCExpr ExprCase        { .. } = CExprCall "match" $ (toCExpr expr):(map mkLambda cases)
    where mkLambda Case    { .. } = CExprLambda (getArgs pat) (toCExpr body)
          getArgs PatCtor  { .. } = argnames
          getArgs PatTuple { .. } = concat $ map getArgs items
          getArgs PatRel   { .. } = [name]
          getArgs PatWild  {}     = ["_"]
toCExpr ExprConstructor { .. } = CExprCall name $ map toCExpr args
toCExpr ExprApply       { func = ExprGlobal { .. }, .. } = CExprCall name $ map toCExpr args
toCExpr ExprApply       { func = ExprRel    { .. }, .. } = CExprCall name $ map toCExpr args
toCExpr ExprApply       { func = ExprLambda { .. }, .. } = CExprCall (head argnames) (map toCExpr args)
toCExpr ExprApply       { func = ExprCoerce { .. }, .. } = toCExpr $ ExprApply value args
toCExpr ExprRel         { .. } = CExprVar name
toCExpr ExprGlobal      { .. } = CExprVar name
toCExpr ExprCoerce      { .. } = toCExpr value
toCExpr ExprDummy       {}     = CExprVar ""

-- Type compiling, from Coq to C++
toCType :: Typ -> CType
toCType TypVar     { .. }             = CTVar name $ map toCExpr args
toCType TypGlob    { targs = [], .. } = CTBase $ toCTBase name
toCType TypGlob    { .. }             = CTExpr (CTBase name) (map toCType targs)
toCType TypVaridx  { .. }             = CTFree idx
toCType TypDummy   {}                 = CTAuto
toCType TypUnknown {}                 = CTAuto
toCType t          {- TypArrow -}     = CTFunc (last . flattenType $ t) (init . flattenType $ t)
-- TODO: Does not take Functions as first-class args for now, flattens everything
-- ie : A -> (A -> B) -> A = [A, A, B, A]
    where flattenType TypArrow { .. } = flattenType left ++ flattenType right 
          flattenType t               = [toCType t]
