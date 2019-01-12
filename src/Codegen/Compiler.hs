{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
module Codegen.Compiler (compile) where
import Parser.Mod
import Parser.Decl
import Parser.Fix
import Parser.Expr
import CIR.File
import CIR.Decl
import CIR.Expr
import Codegen.Expr
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

-- Compile a module
compile :: Module -> State (Context CType) CFile
compile Module { .. } = do
    let alldecls = map toCDecl declarations
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

-- Compile a Coq expression to a C Expression
compilexpr :: Expr -> CExpr
compilexpr e
    | isSeq ce  = ce
    | otherwise = CExprCall (mkdef "return") [ce]
    where ce    = semantics . toCExpr $ e
          isSeq CExprSeq { .. } = True
          isSeq _ = False

-- Add types to generated CDecl by type inference based on a type context
typeify :: CDecl -> State (Context CType) CDecl
-- typeify d | trace ("Typeifying CDecl " ++ show d) False = undefined
typeify CDFunc { .. } = do
    ctx <- get
    let exprmodifier = highorder _fargs . templatify ctx . unify ctx (gettype _fd)
    return $ CDFunc _fd _fargs (exprmodifier  _fbody)
typeify CDSeq { .. } = CDSeq <$> typeify _left <*> typeify _right
typeify o = return o

makemain :: CDecl -> CDecl
makemain CDFunc { _fd = CDef { _nm = "main", _ty = CTExpr "proc" [CTBase "void"] }, _fargs = [], .. } =
    CDFunc (CDef "main" $ CTBase "int") [] $ mkbody _fbody
    where mkbody CExprCall { _cd = CDef { _nm = "return" }, _cparams = [a] } = CExprSeq a retz
          mkbody CExprSeq  { .. } = CExprSeq _left $ mkbody _right
          mkbody o = CExprSeq o retz
          retz = CExprVar "0"
makemain d = omap makemain d

-- Remove coq_ prefix for things in Context
namelink :: Context CType -> CExpr -> CExpr
namelink ctx CExprCall { _cd = CDef { .. }, .. }
    | cannonical `M.member` ctx = CExprCall (CDef cannonical _ty) recparams
    | otherwise = CExprCall (CDef _nm _ty) recparams
    where recparams = fmap (namelink ctx) _cparams
          cannonical = case T.stripPrefix "coq_" _nm of
            (Just cannon) -> cannon
            (Nothing) -> T.toLower _nm
namelink ctx other = omap (namelink ctx) other

-- Link names using namelink
link :: CDecl -> State (Context CType) CDecl
link d = get >>= \ctx -> return $ reexpr (namelink ctx) d
    where reexpr f CDFunc  { .. } = CDFunc _fd _fargs $ f _fbody
          reexpr f CDExpr  { .. } = CDExpr _en $ f _expr
          reexpr f CDSeq   { .. } = (reexpr f _left) <> (reexpr f _right)
          reexpr f d = d

-- Declarations to C Function
toCDecl :: Declaration -> CDecl
-- Fixpoint Declarations -> C Functions
toCDecl FixDecl { fixlist = [ Fix { name = Just nm, value = ExprLambda { .. }, .. } ] } =
    case toCType ftyp of
      (CTFunc { .. }) -> CDFunc (CDef nm . addPtr $ _fret) (zipf argnames . map addPtr $ _fins) cbody
      (e) -> error $ "Fixpoint with non-function type " ++ show e
    where cbody = compilexpr body
-- Lambda Declarations -> C Functions
toCDecl TermDecl { val = ExprLambda { .. }, .. } =
    case toCType typ of
      (CTFunc { .. }) -> CDFunc (CDef name . addPtr $ _fret) (zipf argnames . map addPtr $ _fins) cbody
      -- A constant term declaration is defined as a function with no args and the expression as the body
      (e) -> CDFunc (CDef name e) [] cbody
    where cbody = compilexpr body
-- If an Ind of base is defined as a base type, ignore
toCDecl IndDecl  { .. }
    | name `elem` Conf.base = CDEmpty
    | (T.toLower name) `elem` Conf.base = CDEmpty
-- Inductive type
toCDecl i@IndDecl  { .. } = contract . expand $ i
-- Type Declarations
toCDecl TypeDecl { .. } = CDType . CDef (toCTBase name) $ toCType tval
-- Sanitize declarations for correctness
toCDecl FixDecl { fixlist = [ Fix { name = Just _, value = _ } ] } = error "Fixpoint not followed by an ExprLambda is undefined behavior"
toCDecl FixDecl { fixlist = [ Fix { name = Nothing, .. } ] }       = error "Anonymous Fixpoints are undefined behavior"
toCDecl FixDecl { fixlist = [] }                                   = error "Empty fixlist for declaration found, undefined behavior"
toCDecl FixDecl { fixlist = _:_ }                                  = error "Fixlist with multiple fixpoints is undefined behavior"
toCDecl _ = mempty
