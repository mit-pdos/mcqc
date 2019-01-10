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

-- Compile a Coq expression to a C Expression
compilexpr :: Expr -> CExpr
compilexpr e
    | isSeq ce  = ce
    | otherwise = CExprCall (mkdef "return") [ce]
    where ce    = semantics . toCExpr $ e
          isSeq CExprSeq { .. } = True
          isSeq _ = False

-- Compile a module
compile :: Module -> State (Context CType) CFile
compile Module { .. } = do
    let alldecls = map toCDecl declarations
    -- Get the context so far
    ctx <- get
    let untyped = filter (\d -> not $ getname d `M.member` ctx) alldecls
    -- Add declarations
    let newctx = foldl addctx ctx untyped
    let incls = L.sort . L.nub . concatMap (filterAllowed . getincludes) $ alldecls
    put newctx
    CFile incls . mconcat <$> otraverse typeify untyped


-- Add types to generated CDecl by type inference based on a type context
typeify :: CDecl -> State (Context CType) CDecl
-- typeify d | trace ("Typeifying CDecl " ++ show d) False = undefined
typeify CDFunc { .. } = do
    ctx <- get
    let exprmodifier = templatify ctx . unify ctx (gettype _fd)
    return $ CDFunc _fd _fargs (exprmodifier  _fbody)
typeify CDSeq { .. } = CDSeq <$> typeify _left <*> typeify _right
typeify o = return o

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
