{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Classes.Compilable where
import Parser.Mod
import Parser.Decl
import Parser.Expr
import CIR.File
import CIR.Decl
import CIR.Expr
import Common.Utils
import Classes.Typeful
import Codegen.Rewrite
import Codegen.Top
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
        let untyped = filter (\d -> not $ isUnifiable ctx d) alldecls
        -- Nullary functions are not objects
        let nullary = filter (/= mempty) . map (\case
                CDFunc { _fargs = [], .. } -> _nm _fd
                _ -> mempty) $ untyped
        -- Make nullary objects calls
        -- Link with context
        linked <- otraverse link . map (emap (mknullary nullary)) $ untyped
        -- Add declarations
        let newctx = foldl addctx ctx linked
        let incls = L.sort . L.nub . concatMap (filter (`elem` Conf.libs) . getincludes) $ alldecls
        put newctx
        typed <- otraverse typeify linked
        return . CFile incls . mconcat $ typed
        where mknullary nullfns CExprVar { .. }
                | _var `elem` nullfns = CExprCall (mkauto _var) []
                | otherwise = CExprVar _var
              mknullary nullfns e = omap (mknullary nullfns) e

-- Declarations to C Function
instance Compilable Declaration CDecl where
    -- Fixpoint Declarations -> C Functions
    comp FixDecl { fixlist = [ Fix { name = Just nm, body = ExprLambda { .. }, .. } ] } =
        case comp typ of
          (CTFunc { .. }) -> CDFunc (CDef nm . addPtr $ _fret) (zipf argnames . map addPtr $ _fins) cbody
          (e) -> error $ "Fixpoint with non-function type " ++ show e
        where cbody = semantics . comp $ body
    -- Main function is special
    comp TermDecl { val = ExprLambda { .. }, name = "main", .. } =
        case comp typ of
          (CTExpr "io" [CTBase "void"]) -> CDFunc (CDef "main" $ CTBase "int") [] cbody
          (e) -> error $ "Main function of non 'io<void>' type " ++ show e
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
    comp FixDecl { fixlist = [ Fix { name = Just _, body = _ } ] } = error "Fixpoint not followed by an ExprLambda is undefined behavior"
    comp FixDecl { fixlist = [ Fix { name = Nothing, .. } ] }      = error "Anonymous Fixpoints are undefined behavior"
    comp FixDecl { fixlist = [] }                                  = error "Empty fixlist for declaration found, undefined behavior"
    comp FixDecl { fixlist = _:_ }                                 = error "Fixlist with multiple fixpoints is undefined behavior"
    comp _ = mempty

-- Expression compiling, from Coq to C++
instance Compilable Expr CExpr where
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
    comp ExprApply       { func = ExprVar { .. }, .. } = CExprCall (mkauto name) $ map comp args
    comp ExprApply       { func = ExprLambda { argnames = h:_, .. }, .. } = CExprCall (mkauto h) $ map comp args
    comp ExprApply       { func = ExprCoerce { .. }, .. } = comp $ ExprApply value args
    -- XXX: Outline ExprFix like here
    comp ExprApply       { func = ExprFix { funcs = [Fix { name = Just nm, .. }] }, .. } = fixpoint <> call
        where fixpoint = CExprStmt (mkauto nm) $ comp body
              call = CExprCall (mkauto nm) $ map comp args
    comp ExprLet         { .. } = assignment <> (comp body)
        where assignment = CExprStmt (mkauto name) $ comp nameval
    comp ExprFix         { funcs = [ Fix { name = Just nm, .. } ] } = CExprStmt (mkauto nm) $ comp body
    comp ExprFix         { .. } = error $ "Unsure what to do with " ++ show funcs
    comp ExprVar         { .. } = CExprVar . toCName $ name
    comp ExprCoerce      { .. } = comp value
    comp ExprDummy       {}     = CExprVar ""
    comp e = error $ "Unsure how to compile expr " ++ show e

instance Compilable Type CType where
    comp = toCTypeAbs []

