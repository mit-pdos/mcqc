module Sema.Pipeline where
import CIR.Expr
import Sema.Nat
import Sema.Proc
import Sema.Bool
import Sema.String
import Sema.Pair

semantics :: CExpr -> CExpr
semantics = procSemantics .
            pairSemantics .
            stringSemantics .
            asciiSemantics .
            natSemantics .
            boolSemantics
