{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Codegen.Compilable where
import Parser.Mod
import Parser.Decl
import Parser.Expr
import CIR.File
import CIR.Decl
import CIR.Expr
import Common.Filter
import Common.Utils
import Parser.Pattern
import Codegen.Rewrite
import Codegen.Utils
import Codegen.Ind
import Sema.Pipeline
import Data.MonoTraversable
import Control.Monad.State.Lazy
import qualified Data.List  as L
import qualified Data.Text  as T
import qualified Common.Config as Conf

-- Compilable relation, a compiles to b
class Compilable a b | a -> b where
    comp :: a -> b

-- Compile a module
instance Compilable Module (Env CFile) where
    comp Module { .. } = do
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
        return . CFile incls . mconcat $ typed

-- Declarations to C Function
instance Compilable Declaration CDecl where
    comp :: Declaration -> CDecl
    -- Fixpoint Declarations -> C Functions
    comp FixDecl { fixlist = [ FixD { oname = Just nm, value = ExprLambda { .. }, .. } ] } =
        case comp ftyp of
          (CTFunc { .. }) -> CDFunc (CDef nm . addPtr $ _fret) (zipf argnames . map addPtr $ _fins) cbody
          (e) -> error $ "Fixpoint with non-function type " ++ show e
        where cbody = semantics . comp $ body
    -- Main function is special
    comp TermDecl { val = ExprLambda { .. }, name = "main", .. } =
        case comp typ of
          (CTExpr "proc" [CTBase "void"]) -> CDFunc (CDef "main" $ CTBase "int") [] cbody
          (e) -> error $ "Main function of non 'proc<void>' type " ++ show e
        where mkbody CExprCall { _cd = CDef { _nm = "return" }, _cparams = [a] } = CExprSeq a retz
              mkbody CExprSeq  { .. } = CExprSeq _left $ mkbody _right
              mkbody o = CExprSeq o retz
              retz = CExprCall (mkauto "return") [CExprVar "0"]
              cbody = mkbody . semantics . comp $ body
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

-- Expression compiling, from Coq to C++
instance Compilable Expr CExpr where
    comp :: Expr -> CExpr
    comp ExprLambda      { .. } = CExprLambda defs $ comp body
        where defs = map mkauto argnames
    comp ExprCase        { .. } = CExprCall (mkauto "match") $ comp expr:map mkLambda cases
        where mkLambda Case    { .. } = CExprLambda (mkautos pat) $ comp body
              mkautos                  = map mkauto . getArgs
              getArgs PatCtor  { .. } = argnames
              getArgs PatTuple { .. } = concatMap getArgs items
              getArgs PatRel   { .. } = [name]
              getArgs PatWild  {}     = ["_"]
    comp ExprConstructor { .. } = CExprCall (mkauto name) $ map comp args
    comp ExprApply       { func = ExprGlobal { .. }, .. } = CExprCall (mkauto name) $ map comp args
    comp ExprApply       { func = ExprRel    { .. }, .. } = CExprCall (mkauto name) $ map comp args
    comp ExprApply       { func = ExprLambda { argnames = h:_, .. }, .. } = CExprCall (mkauto h) $ map comp args
    comp ExprApply       { func = ExprCoerce { .. }, .. } = comp $ ExprApply value args
    comp ExprApply       { func = ExprFix { funcs = [FixE { .. }] }, .. } = fixpoint <> call
        where fixpoint = CExprStmt (mkauto name) $ comp body
              call = CExprCall (mkauto name) $ map comp args
    comp ExprLet         { .. } = assignment <> (comp body)
        where assignment = CExprStmt (mkauto name) $ comp nameval
    comp ExprFix         { funcs = [ FixE { .. } ] } = CExprStmt (mkauto name) $ comp body
    comp ExprFix         { .. } = error $ "Unsure what to do with " ++ show funcs
    comp ExprRel         { .. } = CExprVar . toCName $ name
    comp ExprGlobal      { .. } = CExprVar . toCName $ name
    comp ExprCoerce      { .. } = comp value
    comp ExprDummy       {}     = CExprVar ""

instance Compilable Typ CType where
    comp :: Typ -> CType
    comp = toCTypeAbs []

