{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
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
import Common.Utils
import Sema.Pipeline
import Control.Monad.State
import Data.Text (Text)
import qualified Data.List     as L
import qualified Data.Text     as T
import qualified Data.Map      as M
import qualified Common.Config as Conf
import Debug.Trace

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
    let alldecls = concatMap (expandind . toCDecl) $ declarations
    -- Get the context so far
    ctx <- get
    let untyped = filter (\d -> not $ getname d `M.member` ctx) alldecls
    -- Add declarations
    let newctx = foldl addctx ctx untyped
    let incls = L.sort . L.nub . concatMap getAllowedIncludes $ alldecls
    put newctx
    return . CFile incls $ map (typeInfer newctx) $ untyped

-- Add types to generated CDecl by type inference based on a type context
typeInfer :: Context CType -> CDecl -> CDecl
typeInfer ctx CDFunc { .. } = CDFunc _fd _fargs $ unify ctx (gettype _fd) _fbody
typeInfer _ o = o

-- Get allowed includes based on the libraries in the config
getAllowedIncludes :: CDecl -> [Text]
getAllowedIncludes = filter (`elem` Conf.libs) . getincludes

-- Declarations to C Function
toCDecl :: Declaration -> CDecl
-- Fixpoint Declarations -> C Functions
toCDecl FixDecl { fixlist = [ Fix { name = Just n, value = ExprLambda { .. }, .. } ] } =
    CDFunc retNT argsNT cbody
    where cbody = compilexpr body
          (retNT, argsNT) = case toCType ftyp of
                                (CTFunc { .. }) -> (CDef n _fret, zipf argnames _fins)
                                (o) -> error $ "Fixpoint declartion with no-func type " ++ show o
-- Lambda Declarations -> C Functions
toCDecl TermDecl { val = ExprLambda { .. }, .. } =
    CDFunc retNT argsNT cbody
    where cbody = compilexpr body
          (retNT, argsNT) = case toCType typ of
                                (CTFunc { .. }) -> (CDef name _fret, zipf argnames _fins)
                                (e) -> (CDef name e, [])
-- If an Ind of base is found, ignore
toCDecl IndDecl  { .. }
    | name `elem` Conf.base = CDEmpty
    | (T.toLower name) `elem` Conf.base = CDEmpty
-- Inductive type
toCDecl IndDecl  { iargs = [], .. } = CDInd (CDef iname indtype) $ map mkctor constructors
    where mkctor IndConstructor { .. } = (name, CTFunc indtype $ map (mkptr . toCType) argtypes)
          mkctor o = error $ "Non inductive constructor found, failing " ++ show o
          mkptr t | t == indtype = CTPtr t | otherwise = t
          indtype = CTBase iname
          iname = toCTBase name
-- Parametric inductive type
toCDecl IndDecl  { .. } = CDInd (CDef iname indtype) $ map mkctor constructors
    where mkctor IndConstructor { .. } = (name, CTFunc indtype $ map (mkptr . toCTypeAbs iargs) argtypes)
          mkctor o = error $ "Non inductive constructor found, failing " ++ show o
          mkptr t | t == indtype = CTPtr t | otherwise = t
          indtype  = CTExpr iname [CTFree $ length iargs]
          iname = toCTBase name
-- Type Declarations
toCDecl TypeDecl { .. } = CDType . CDef (toCTBase name) $ toCType tval
-- Sanitize declarations for correctness
toCDecl FixDecl { fixlist = [ Fix { name = Just _, value = _ } ] } = error "Fixpoint not followed by an ExprLambda is undefined behavior"
toCDecl FixDecl { fixlist = [ Fix { name = Nothing, .. } ] }       = error "Anonymous Fixpoints are undefined behavior"
toCDecl FixDecl { fixlist = [] }                                   = error "Empty fixlist for declaration found, undefined behavior"
toCDecl FixDecl { fixlist = _:_ }                                  = error "Fixlist with multiple fixpoints is undefined behavior"

