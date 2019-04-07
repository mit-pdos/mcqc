module Sema.Pipeline where
import CIR.Expr
import Sema.Nat
import Sema.IO
import Sema.Bool
import Sema.String
import Sema.Pair

semantics :: CExpr -> CExpr
semantics = ioSemantics .
            pairSemantics .
            stringSemantics .
            asciiSemantics .
            natSemantics .
            boolSemantics
