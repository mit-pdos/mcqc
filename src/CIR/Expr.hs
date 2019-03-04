{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
module CIR.Expr where
import GHC.Generics
import Data.Aeson
import Data.MonoTraversable
import Codegen.Rewrite
import Data.Text (Text)

-- Foldable is not possible as CExpr cannot be empty, FunctorM will do
class MonoFunctorM mono where
    omapM :: Monad m => (Element mono -> m (Element mono)) -> mono -> m mono

-- C++ Typed names
data CDef = CDef { _nm :: Text, _ty :: CType }
    deriving (Show, Eq, Generic, ToJSON)

-- C++ Types
data CType =
    CTFunc    { _fret :: CType, _fins :: [CType] }
    | CTExpr  { _tbase :: Text, _tins :: [CType] }
    | CTVar   { _vname :: Text, _vargs :: [CExpr] }
    | CTBase  { _base :: Text }
    | CTPtr   { _inner :: CType }
    | CTFree  { _idx :: Int }
    | CTAuto  {}
    deriving (Show, Eq, Generic, ToJSON)

-- Make infix syntactic sugar
(-->) :: [CType] -> CType -> CType
args --> ret = CTFunc ret args
infixl 8 -->

-- C++ Expressions
data CExpr =
          -- High level C++ expressions
            CExprLambda { _lds :: [CDef], _lbody :: CExpr }
          | CExprCall   { _cd :: CDef, _cparams :: [CExpr] }
          -- Continuations
          | CExprSeq    { _left :: CExpr, _right :: CExpr }
          -- C++ statament
          | CExprStmt   { _sd :: CDef, _sbody :: CExpr }
          -- Reduced forms
          | CExprVar    { _var :: Text }
          | CExprStr    { _str :: Text }
          | CExprNat    { _nat :: Int }
          | CExprBool   { _bool :: Bool }
          | CExprPair   { _fst :: CExpr, _snd :: CExpr }
    deriving (Show, Eq, Generic, ToJSON)

instance Semigroup CExpr where
    (<>) = CExprSeq

-- Compatible with Element a = a
type instance Element CExpr = CExpr
type instance Element CType = CType
type instance Element CDef  = CDef

-- CTypes are monomorphic functors
instance MonoFunctor CType where
    omap f   CTFunc { .. } = fmap f _fins --> f _fret
    omap f   CTExpr { .. } = CTExpr _tbase $ fmap f _tins
    omap f   CTPtr  { .. } = CTPtr $ f _inner
    omap _   CTVar  { .. } = error $ "Type:var with CExpr subterm cannot be traversed " ++ show _vname
    omap _   other         = other

-- CExpr are monomorphic functors
instance MonoFunctor CExpr where
    omap f   CExprCall   { .. } = CExprCall _cd $ fmap f _cparams
    omap f   CExprStmt   { .. } = CExprStmt _sd $ f _sbody
    omap f   CExprSeq    { .. } = CExprSeq (f _left) (f _right)
    omap f   CExprPair   { .. } = CExprPair (f _fst) (f _snd)
    omap f   CExprLambda { .. } = CExprLambda _lds $ f _lbody
    -- If it doesn't match anything, then it's a normal form, ignore
    omap _   other              = other

instance MonoFunctorM CExpr where
    omapM f   CExprCall   { .. } = mapM f _cparams >>= \ps -> return $ CExprCall _cd ps
    omapM f   CExprStmt   { .. } = f _sbody >>= \b -> return $ CExprStmt _sd b
    omapM f   CExprLambda { .. } = f _lbody >>= \b -> return $ CExprLambda _lds b
    omapM f   CExprSeq    { .. } = do { l <- f _left; r <- f _right; return $ CExprSeq l r }
    omapM f   CExprPair   { .. } = do { l <- f _fst; r <- f _snd; return $ CExprPair l r }
    -- If it doesn't match anything, then it's a normal form, ignore
    omapM _   other              = return other

-- Convert sequence to list of expressions
seqToList :: CExpr -> [CExpr]
seqToList CExprSeq { .. } = _left:seqToList _right
seqToList other           = [other]

-- Convert a list of expressions to a sequence
listToSeq :: [CExpr] -> CExpr
listToSeq []     = error "Empty sequence list given, unable to convert to expression"
listToSeq [a]    = a
listToSeq (a:ts) = a <> listToSeq ts

-- Make an untyped definition
mkauto :: Text -> CDef
mkauto nm = CDef (toCName nm) CTAuto

