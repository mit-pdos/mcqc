{-# LANGUAGE TemplateHaskell, DeriveAnyClass, DeriveGeneric   #-}
module CIR.Expr where
import GHC.Generics
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
    | CTUndef {} -- Should never output this type, means type inference failed
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
          | CExprList { _etype :: CType, _elems :: [CExpr] }
          | CExprTuple { _items :: [CExpr] }
          -- Continuations
          | CExprSeq { _left :: CExpr, _right :: CExpr }
    deriving (Eq, Generic, ToJSON, Show)

-- Generate lenses
makeLenses ''CExpr
makeLenses ''CType

