module Sema.Pipeline where
import CIR.Expr
import Sema.Nat
import Sema.Proc
import Sema.Bool
import Sema.String
import Sema.Tuple

semantics :: CExpr -> CExpr
semantics = procSemantics .
            tupleSemantics .
            stringSemantics .
            asciiSemantics .
            natSemantics .
            boolSemantics
