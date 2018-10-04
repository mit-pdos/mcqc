{-# LANGUAGE RecordWildCards #-}
module Common.Flatten where
import CIR.Expr
import CIR.Decl
import Codegen.Rewrite
import Control.Monad.State
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Common.Config as Conf
import Control.Lens
import Debug.Trace

-- Propagate to children expr
descend :: (CExpr -> CExpr) -> CExpr -> CExpr
descend f   CExprCall   { .. } = CExprCall _fname $ map f _fparams
descend f   CExprStmt   { .. } = CExprStmt _stype _sname $ f _sbody
descend f   CExprSeq    { .. } = CExprSeq (f _left) (f _right)
descend f   CExprTuple  { .. } = CExprTuple $ map f _items
descend f   CExprList   { .. } = CExprList _etype $ map f _elems
descend f   CExprOption { .. } = CExprOption _otype $ fmap f _val
descend f   CExprLambda { .. } = CExprLambda _largs $ f _lbody
-- If it doesn't match anything, then it's a normal form, ignore
descend _   other              = other

-- Monadic, propagate to children expr
descendM :: Monad m => (CExpr -> m CExpr) -> CExpr -> m CExpr
descendM f   CExprCall   { .. } = mapM f _fparams >>= \ps -> return $ CExprCall _fname ps
descendM f   CExprStmt   { .. } = f _sbody >>= \b -> return $ CExprStmt _stype _sname b
descendM f   CExprLambda { .. } = f _lbody >>= \b -> return $ CExprLambda _largs b
descendM f   CExprSeq    { .. } = do { l <- f _left; r <- f _right; return $ CExprSeq l r }
descendM f   CExprTuple  { .. } = mapM f _items >>= \items -> return $ CExprTuple items
descendM f   CExprList   { .. } = mapM f _elems >>= \elems -> return $ CExprList _etype elems
descendM f   CExprOption { _val = Nothing, .. } = return $ CExprOption _otype Nothing
descendM f   CExprOption { _val = Just a, .. } = f a >>= \b -> return $ CExprOption _otype (Just b)
-- If it doesn't match anything, then it's a normal form, ignore
descendM _   other              = return other


-- Convert sequence to list
seqToList :: CExpr -> [CExpr]
seqToList CExprSeq { .. } = _left:seqToList _right
seqToList other           = [other]

-- Convert a list of expressions to a sequence
listToSeq :: [CExpr] -> CExpr
listToSeq []     = error "Empty sequence list given, unable to convert to expression"
listToSeq [a]    = a
listToSeq (a:ts) = CExprSeq a $ listToSeq ts

-- Apply toCName to a CExpr
renames :: CExpr -> CExpr
renames =
 -- single step lenses
 over fname toCName
 . over str toCName
 . over var toCName
 -- nested definition lenses
 . over (largs . traverse) toCName
 -- recursive lenses
 . over lbody renames
 . over sbody renames
 . over left renames
 . over right renames
 . over (items . traverse) renames
 . over (fparams . traverse) renames
 . over (items . traverse) renames
 . over (elems . traverse) renames
 . over (val . traverse) renames

