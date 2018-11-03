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
import Codegen.Ind
import Codegen.Rewrite
import Types.Inference
import Types.Flatten
import Memory.Copy
import Common.Utils
import Sema.Pipeline
import Data.MonoTraversable
import qualified Data.List as L

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
          untypdecls = concatMap (expandind . toCDecl) declarations
          incls = L.sort . L.nub . concat $ map getIncludes untypdecls

-- Add types to generated CDecl by type inference based on a type context
typeInfer :: Context CType -> CDecl -> CDecl
typeInfer ctx CDFunc { _fd = fd@CDef { .. }, .. } = CDFunc fd _fargs $ unifyExpr ctx _ty _fbody
typeInfer _ o = o

-- Declarations to C Function
toCDecl :: Declaration -> CDecl
-- Fixpoint Declarations -> C Functions
toCDecl FixDecl { fixlist = [ Fix { name = Just n, value = ExprLambda { .. }, .. } ] } =
    CDFunc retNT argsNT cbody
    where cbody = copyopt argnames . compilexpr $ body
          (retNT, argsNT) = case toCType ftyp of
                                (CTFunc { .. }) -> (CDef n _fret, zipf argnames _fins)
                                (o) -> error $ "Fixpoint declartion with no-func type " ++ show o
-- Lambda Declarations -> C Functions
toCDecl TermDecl { val = ExprLambda { .. }, .. } =
    CDFunc retNT argsNT cbody
    where cbody = copyopt argnames . compilexpr $ body
          (retNT, argsNT) = case toCType typ of
                                (CTFunc { .. }) -> (CDef name _fret, zipf argnames _fins)
                                (o) -> error $ "Function declartion with no-func type " ++ show o
-- Inductive type
toCDecl IndDecl  { iargs = [], .. } = CDInd (CDef iname indtype) $ map mkctor constructors
    where mkctor IndConstructor { .. } = (name, CTFunc indtype $ map (mkptr . toCType) argtypes)
          mkctor o = error $ "Non inductive constructor found, failing " ++ show o
          mkptr t | t == indtype = CTPtr t | otherwise = t
          indtype = CTBase iname
          iname = toCTBase name
-- Parametric inductive type
toCDecl IndDecl  { .. }             = CDInd (CDef iname indtype) $ map mkctor constructors
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

