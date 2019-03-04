{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards #-}
module Codegen.Utils where
import CIR.Expr
import CIR.Decl
import Codegen.Rewrite
import Types.Templates
import Types.Context
import Data.MonoTraversable
import Control.Monad.State
import qualified Data.Map      as M

-- Environment monad
type Env a = State (Context CType) a

-- Add types to generated CDecl by type inference based on a type context
typeify :: CDecl -> Env CDecl
-- typeify d | trace ("Typeifying CDecl " ++ show d) False = undefined
typeify CDFunc { .. } = do
    ctx <- get
    let exprmodifier = plug freedom . highorder _fargs . templatify ctx . unify ctx (gettype _fd)
    return $ CDFunc _fd _fargs (exprmodifier  _fbody)
    where freedom = maximum $ fmap getMaxVaridx (_fd:_fargs)
typeify CDSeq { .. } = CDSeq <$> typeify _left <*> typeify _right
typeify o = return o

-- Remove coq_ prefix for things in Context
-- TODO: match the type as well as name
namelink :: Context CType -> CExpr -> CExpr
namelink ctx CExprCall { _cd = CDef { .. }, .. } =
   let cannonical = cannonicalizeFn _nm
       newdef = flip CDef _ty $ if cannonical `M.member` ctx then cannonical else _nm in
   CExprCall newdef $ map (namelink ctx) _cparams
namelink ctx other = omap (namelink ctx) other

-- Link names using namelink
link :: CDecl -> Env CDecl
link CDFunc { .. } = get >>= \ctx -> return $ CDFunc _fd _fargs (namelink ctx _fbody)
link CDSeq  { .. } = CDSeq <$> link _left <*> link _right
link d = return d


