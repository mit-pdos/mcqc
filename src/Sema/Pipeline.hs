module Sema.Pipeline where
import Codegen.Expr
import Sema.Nat
import Sema.Proc
import Sema.Bool
import Sema.List
import Sema.Byte

semantics :: CExpr -> CExpr
semantics = procSemantics . listSemantics . asciiSemantics . natSemantics . boolSemantics
