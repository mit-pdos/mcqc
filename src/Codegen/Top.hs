{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards #-}
module Codegen.Top where
import CIR.Expr
import CIR.Decl
import Classes.Typeful
import Codegen.Rewrite
import Types.Templates
import Types.Context
import Data.MonoTraversable
import Control.Monad.State
import Data.Maybe

-- Environment monad
type Env a = State (Context CType) a

-- Add types to generated CDecl by type inference based on a type context
typeify :: CDecl -> Env CDecl
-- typeify d | trace ("Typeifying CDecl " ++ show d) False = undefined
typeify CDFunc { .. } = do
    ctx <- get
    let ftype = gettype _fd
        typedbody = fromMaybe _fbody . unify ctx ftype $ _fbody
        exprmodifier = plug freedom . highorder _fargs . templatify ctx
    return $ CDFunc _fd _fargs (exprmodifier typedbody)
    where freedom = maximum $ fmap getMaxVaridx (_fd:_fargs)
typeify CDSeq { .. } = CDSeq <$> typeify _left <*> typeify _right
typeify o = return o

-- Remove coq_ prefix for things in Context
namelink :: Context CType -> CExpr -> CExpr
namelink ctx c@CExprCall { _cd = d@CDef { .. }, .. }
    | isUnifiable ctx c = CExprCall newD $ map (namelink ctx) _cparams
    | otherwise = CExprCall d $ map (namelink ctx) _cparams
    where newD = CDef (cannonicalizeFn _nm) _ty
namelink ctx other = omap (namelink ctx) other

-- Link names using namelink
link :: CDecl -> Env CDecl
link CDFunc { .. } = get >>= \ctx -> return $ CDFunc _fd _fargs (namelink ctx _fbody)
link CDSeq  { .. } = CDSeq <$> link _left <*> link _right
link d = return d

