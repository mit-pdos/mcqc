{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module CIR.Expr where
import GHC.Generics
import Control.Lens
import Data.Aeson
import Data.Text (Text)
import Data.Maybe
import Data.MonoTraversable

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
          -- Continuations
          | CExprSeq { _left :: CExpr, _right :: CExpr }
          -- C++ statament
          | CExprStmt { _stype :: CType, _sname :: Text, _sbody :: CExpr }
          -- Reduced forms
          | CExprVar { _var :: Text }
          | CExprStr { _str :: Text }
          | CExprNat { _nat :: Int }
          | CExprBool { _bool :: Bool }
          | CExprList { _etype :: CType, _elems :: [CExpr] }
          | CExprOption { _otype :: CType, _val :: Maybe CExpr }
          | CExprTuple { _items :: [CExpr] }
    deriving (Eq, Generic, ToJSON, Show)

-- Compatible with Element a = a
type instance Element CExpr = CExpr
type instance Element CType = CType

-- CExpr are monomorphic functors and traversables
instance MonoFunctor CExpr where
    omap f   CExprCall   { .. } = CExprCall _fname $ fmap f _fparams
    omap f   CExprStmt   { .. } = CExprStmt _stype _sname $ f _sbody
    omap f   CExprSeq    { .. } = CExprSeq (f _left) (f _right)
    omap f   CExprTuple  { .. } = CExprTuple $ fmap f _items
    omap f   CExprList   { .. } = CExprList _etype $ fmap f _elems
    omap f   CExprOption { .. } = CExprOption _otype $ fmap f _val
    omap f   CExprLambda { .. } = CExprLambda _largs $ f _lbody
    -- If it doesn't match anything, then it's a normal form, ignore
    omap _   other              = other

-- Foldable is not possible as CExpr has no mempty, FunctorM will do
class MonoFunctorM mono where
    omapM :: Monad m => (Element mono -> m (Element mono)) -> mono -> m mono

instance MonoFunctorM CExpr where
    -- No foldable instance for CExpr, use omapM out of class
    -- omapM :: Monad m => (CExpr -> m CExpr) -> CExpr -> m CExpr
    omapM f   CExprCall   { .. } = mapM f _fparams >>= \ps -> return $ CExprCall _fname ps
    omapM f   CExprStmt   { .. } = f _sbody >>= \b -> return $ CExprStmt _stype _sname b
    omapM f   CExprLambda { .. } = f _lbody >>= \b -> return $ CExprLambda _largs b
    omapM f   CExprSeq    { .. } = do { l <- f _left; r <- f _right; return $ CExprSeq l r }
    omapM f   CExprTuple  { .. } = mapM f _items >>= \items -> return $ CExprTuple items
    omapM f   CExprList   { .. } = mapM f _elems >>= \elems -> return $ CExprList _etype elems
    omapM f   CExprOption { _val = Nothing, .. } = return $ CExprOption _otype Nothing
    omapM f   CExprOption { _val = Just a, .. } = f a >>= \b -> return $ CExprOption _otype (Just b)
    -- If it doesn't match anything, then it's a normal form, ignore
    omapM _   other              = return other

-- Convert sequence to list
seqToList :: CExpr -> [CExpr]
seqToList CExprSeq { .. } = _left:seqToList _right
seqToList other           = [other]

-- Convert a list of expressions to a sequence
listToSeq :: [CExpr] -> CExpr
listToSeq []     = error "Empty sequence list given, unable to convert to expression"
listToSeq [a]    = a
listToSeq (a:ts) = CExprSeq a $ listToSeq ts

-- Generate lenses
makeLenses ''CExpr
makeLenses ''CType

