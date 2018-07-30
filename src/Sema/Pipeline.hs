module Sema.Pipeline where
import Codegen.Expr
import Sema.Nat
import Sema.Proc
import Sema.Bool
import Sema.List

semantics :: CExpr -> CExpr
semantics = procSemantics . listSemantics . natSemantics . boolSemantics
