{-# LANGUAGE RecordWildCards, OverloadedStrings  #-}
module Codegen.Compiler where
import Parser.Mod
import Parser.Decl
import Parser.Fix
import Parser.Expr
import CIR.File
import CIR.Decl
import CIR.Expr
import Codegen.Expr
import Codegen.Rewrite
import Common.Flatten
import Sema.Pipeline
import Control.Lens
import Data.Text (Text)
import Data.List (sort, nub)
import Debug.Trace

-- TODO: Ignore used modules for now
compile :: Module -> CFile
compile Module { .. } = CFile incls cdecls
    where cdecls = map (optimize . toCDecl) declarations
          incls = sort . nub . concat $ map getLibs cdecls

-- optimize pipeline
optimize :: CDecl -> CDecl
optimize = over fbody (renames . semantics)

-- Declarations to C Function
toCDecl :: Declaration -> CDecl
-- toCDecl d | trace ("=== DBG Compiler.hs/toCDecl " ++ show d) False = undefined
-- Define fixpoint by translating it to a single return statement
toCDecl FixDecl { fixlist = [ Fix { name = Just n, value = ExprLambda { .. }, .. } ] } =
    CDFunc n funcT argnames cbody
    where cbody = CExprCall "return" [toCExpr body]
          funcT = toCType ftyp
-- Define imperative function by unrolling the proc monad to sequential statement expressions
toCDecl TermDecl { val = ExprLambda { .. }, .. } =
    CDFunc name funcT argnames cbody
    where cbody = toCExpr body
          funcT = toCType typ
-- Define type definitions
toCDecl TypeDecl { .. } = CDType (toCTBase name) $ toCType tval
-- Sanitize declarations for correctness
toCDecl FixDecl { fixlist = [ Fix { name = Just n, value = l } ] } = error "Fixpoint not followed by an ExprLambda is undefined behavior"
toCDecl FixDecl { fixlist = [ Fix { name = Nothing, .. } ] }       = error "Anonymous Fixpoints are undefined behavior"
toCDecl FixDecl { fixlist = [] }                                   = error "Empty fixlist for declaration found, undefined behavior"
toCDecl FixDecl { fixlist = f:fl }                                 = error "Fixlist with multiple fixpoints is undefined behavior"
-- XXX: Implement other declarations
toCDecl IndDecl  { .. } = CDEmpty {}
