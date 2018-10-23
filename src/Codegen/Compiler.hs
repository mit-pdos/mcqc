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
import Debug.Trace

{-
 CDFunc { _fn :: Text, _ftype :: CType, _fargs :: [Text], _fbody :: CExpr }
    | CDType { _tname :: Text, _tval :: CType }
    | CDExpr { _ename :: Text, _expr :: CExpr }
    | CDEmpty { }
  deriving (Eq, Generic, ToJSON)
-}

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
compile classCtx Module { .. } = CFile incls cdecls
    where declcomp = typeInfer classCtx . toCDecl
          cdecls = map declcomp declarations
          incls = sort . nub . concat $ map getIncludes cdecls

-- Add types to generated CDecl by type inference based on a type context
-- TODO: WIP use ctx
typeInfer :: Context CType -> CDecl -> CDecl
typeInfer ctx CDFunc { .. } = CDFunc _fn _ftype _fargs $ plugInExpr (_fret _ftype) _fbody
typeInfer ctx o = o

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
-- Type Declarations
toCDecl TypeDecl { .. } = CDType (toCTBase name) $ toCType tval
-- Sanitize declarations for correctness
toCDecl FixDecl { fixlist = [ Fix { name = Just n, value = l } ] } = error "Fixpoint not followed by an ExprLambda is undefined behavior"
toCDecl FixDecl { fixlist = [ Fix { name = Nothing, .. } ] }       = error "Anonymous Fixpoints are undefined behavior"
toCDecl FixDecl { fixlist = [] }                                   = error "Empty fixlist for declaration found, undefined behavior"
toCDecl FixDecl { fixlist = f:fl }                                 = error "Fixlist with multiple fixpoints is undefined behavior"
-- TODO: Implement inductive declarations
toCDecl IndDecl  { .. } = CDEmpty {}
