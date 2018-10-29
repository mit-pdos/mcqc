{-# LANGUAGE OverloadedStrings  #-}
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
import Codegen.Rewrite
import Types.Inference
import Types.Flatten
import Memory.Copy
import Sema.Pipeline
import Data.Text (Text)
import Data.List (sort, nub)
import Data.MonoTraversable

-- Compile a Coq expression to a C Expression
compilexpr :: Expr -> CExpr
compilexpr e
    -- | trace ("DBG compiling " ++ show ce) False = undefined
    | isSeq ce  = ce
    | otherwise = CExprCall "return" [ce]
    where ce    = annotate . renames . semantics . toCExpr $ e
          isSeq CExprSeq { .. } = True
          isSeq _ = False
          annotate CExprLambda { .. } = CExprLambda _largs $ copyopt _largs _lbody
          annotate o = omap annotate o

-- TODO: Ignore imported modules for now
compile :: Context CType -> Module -> CFile
compile ctx Module { .. } = CFile incls $ map (typeInfer newctx) untypdecls
    where newctx = foldl addCtx ctx untypdecls
          untypdecls = map toCDecl declarations
          incls = sort . nub . concat $ map getIncludes untypdecls

-- Add types to generated CDecl by type inference based on a type context
-- TODO: WIP use ctx
typeInfer :: Context CType -> CDecl -> CDecl
typeInfer ctx CDFunc { _ftype = ft@CTFunc { .. }, .. } = CDFunc _fn ft _fargs $ plugInExpr ctx _fret _fbody
typeInfer ctx CDFunc { .. } = error $ "Function declaration " ++ show _fn ++ " with non-function type " ++ show _ftype
typeInfer _ o = o

-- Declarations to C Function
toCDecl :: Declaration -> CDecl
-- Fixpoint Declarations -> C Functions
toCDecl FixDecl { fixlist = [ Fix { name = Just n, value = ExprLambda { .. }, .. } ] } =
    CDFunc n funcT argnames cbody
    where cbody = copyopt argnames . compilexpr $ body
          funcT = toCType ftyp
-- Lambda Declarations -> C Functions
toCDecl TermDecl { val = ExprLambda { .. }, .. } =
    CDFunc name funcT argnames cbody
    where cbody = copyopt argnames . compilexpr $ body
          funcT = toCType typ
-- Inductive types
toCDecl IndDecl  { iargs = [], .. } = CDInd (toCTBase name) (CTBase . toCTBase $ name) $ map mkctor constructors
    where mkctor IndConstructor { .. } = (name, map toCType argtypes)
          mkctor o = error $ "Non inductive constructor found, failing " ++ show o
toCDecl IndDecl  { .. } = CDInd (toCTBase name) indtype $ map mkctor constructors
    where mkctor IndConstructor { .. } = (name, map (toCTypeAbs iargs) argtypes)
          mkctor o = error $ "Non inductive constructor found, failing " ++ show o
          indtype  = CTExpr (toCTBase name) [CTFree $ length iargs - 1]
-- Type Declarations
toCDecl TypeDecl { .. } = CDType (toCTBase name) $ toCType tval
-- Sanitize declarations for correctness
toCDecl FixDecl { fixlist = [ Fix { name = Just _, value = _ } ] } = error "Fixpoint not followed by an ExprLambda is undefined behavior"
toCDecl FixDecl { fixlist = [ Fix { name = Nothing, .. } ] }       = error "Anonymous Fixpoints are undefined behavior"
toCDecl FixDecl { fixlist = [] }                                   = error "Empty fixlist for declaration found, undefined behavior"
toCDecl FixDecl { fixlist = _:_ }                                  = error "Fixlist with multiple fixpoints is undefined behavior"

