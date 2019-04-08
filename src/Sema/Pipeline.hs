module Sema.Pipeline where
import CIR.Expr
import Sema.Nat
import Sema.IO
import Sema.Bool
import Sema.String
import Sema.Prod

semantics :: CExpr -> CExpr
semantics = ioSemantics .
            prodSemantics .
            stringSemantics .
            asciiSemantics .
            natSemantics .
            boolSemantics
