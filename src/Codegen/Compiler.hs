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
import Common.Flatten
import Memory.Copy
import Sema.Pipeline
import Data.Text (Text)
import Data.List (sort, nub)

-- Compile a Coq expression to a C Expression
compilexpr :: Expr -> CExpr
compilexpr e
    | isSeq ce  = ce
    | otherwise = CExprCall "return" [ce]
    where ce    = copyopt . renames . semantics . toCExpr $ e
          isSeq CExprSeq { .. } = True
          isSeq _               = False

-- TODO: Ignore imported modules for now
compile :: Module -> CFile
compile Module { .. } = CFile incls cdecls
    where cdecls = map toCDecl declarations
          incls = sort . nub . concat $ map getLibs cdecls

-- Declarations to C Function
toCDecl :: Declaration -> CDecl
-- Fixpoint Declarations -> C Functions
toCDecl FixDecl { fixlist = [ Fix { name = Just n, value = ExprLambda { .. }, .. } ] } =
    CDFunc n funcT argnames cbody
    where cbody = maxinsert retT . copyannotate argnames . compilexpr $ body
          funcT = toCType ftyp
          retT  = _fret funcT
-- Lambda Declarations -> C Functions
toCDecl TermDecl { val = ExprLambda { .. }, .. } =
    CDFunc name funcT argnames cbody
    where cbody = maxinsert retT . copyannotate argnames . compilexpr $ body
          funcT = toCType typ
          retT  = _fret funcT
-- Type Declarations
toCDecl TypeDecl { .. } = CDType (toCTBase name) $ toCType tval
-- Sanitize declarations for correctness
toCDecl FixDecl { fixlist = [ Fix { name = Just n, value = l } ] } = error "Fixpoint not followed by an ExprLambda is undefined behavior"
toCDecl FixDecl { fixlist = [ Fix { name = Nothing, .. } ] }       = error "Anonymous Fixpoints are undefined behavior"
toCDecl FixDecl { fixlist = [] }                                   = error "Empty fixlist for declaration found, undefined behavior"
toCDecl FixDecl { fixlist = f:fl }                                 = error "Fixlist with multiple fixpoints is undefined behavior"
-- TODO: Implement other declarations
toCDecl IndDecl  { .. } = CDEmpty {}
