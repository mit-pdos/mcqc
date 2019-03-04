{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
module Codegen.Compiler where
import Parser.Mod
import Parser.Decl
import Parser.Expr
import CIR.File
import CIR.Decl
import CIR.Expr
import Codegen.Ind
import Codegen.Rewrite
import Types.Context
import Types.Templates
import Common.Utils
import Common.Filter
import Sema.Pipeline
import Data.MonoTraversable
import Control.Monad.State
import qualified Data.List     as L
import qualified Data.Text     as T
import qualified Data.Map      as M
import qualified Common.Config as Conf

type Env a = State (Context CType) a

-- Declarations to C Function
instance Compilable Declaration CDecl where
    comp :: Declaration -> CDecl
    -- Fixpoint Declarations -> C Functions
    comp FixDecl { fixlist = [ FixD { oname = Just nm, value = ExprLambda { .. }, .. } ] } =
        case comp ftyp of
          (CTFunc { .. }) -> CDFunc (CDef nm . addPtr $ _fret) (zipf argnames . map addPtr $ _fins) cbody
          (e) -> error $ "Fixpoint with non-function type " ++ show e
        where cbody = semantics . comp $ body
    -- Lambda Declarations -> C Functions
    comp TermDecl { val = ExprLambda { .. }, .. } =
        case comp typ of
          (CTFunc { .. }) -> CDFunc (CDef name . addPtr $ _fret) (zipf argnames . map addPtr $ _fins) cbody
          -- A constant term declaration is defined as a function with no args and the expression as the body
          (e) -> CDFunc (CDef name e) [] cbody
        where cbody = semantics . comp $ body
    -- If an Ind of base is defined as a base type, ignore
    comp IndDecl  { .. }
        | name `elem` Conf.base = CDEmpty
        | (T.toLower name) `elem` Conf.base = CDEmpty
    -- Inductive type
    comp i@IndDecl  { .. } = contract . expand $ i
    -- Type Declarations
    comp TypeDecl { .. } = CDType . CDef (toCTBase name) $ comp tval
    -- Sanitize declarations for correctness
    comp FixDecl { fixlist = [ FixD { oname = Just _, value = _ } ] } = error "Fixpoint not followed by an ExprLambda is undefined behavior"
    comp FixDecl { fixlist = [ FixD { oname = Nothing, .. } ] }       = error "Anonymous Fixpoints are undefined behavior"
    comp FixDecl { fixlist = [] }                                     = error "Empty fixlist for declaration found, undefined behavior"
    comp FixDecl { fixlist = _:_ }                                    = error "Fixlist with multiple fixpoints is undefined behavior"
    comp _ = mempty

-- Compile a module
compile :: Module -> Env CFile
compile Module { .. } = do
    let alldecls = map comp declarations
    -- Get the context so far
    ctx <- get
    let untyped = filterDecl ctx alldecls
    -- Link with context
    linked <- otraverse link untyped
    -- Add declarations
    let newctx = foldl addctx ctx linked
    let incls = L.sort . L.nub . concatMap (filterAllowed . getincludes) $ alldecls
    put newctx
    typed <- otraverse typeify linked
    return . CFile incls . makemain . mconcat $ typed

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

makemain :: CDecl -> CDecl
makemain CDFunc { _fd = CDef { _nm = "main", _ty = CTExpr "proc" [CTBase "void"] }, _fargs = [], .. } =
    CDFunc (CDef "main" $ CTBase "int") [] $ mkbody _fbody
    where mkbody CExprCall { _cd = CDef { _nm = "return" }, _cparams = [a] } = CExprSeq a retz
          mkbody CExprSeq  { .. } = CExprSeq _left $ mkbody _right
          mkbody o = CExprSeq o retz
          retz = CExprCall (mkauto "return") [CExprVar "0"]
makemain d = omap makemain d

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


