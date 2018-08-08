module Sema.Pipeline where
import Codegen.Expr
import Sema.Nat
import Sema.Proc
import Sema.Bool
import Sema.List
import Sema.String
import Sema.Tuple

semantics :: CExpr -> CExpr
semantics = procSemantics .
            listSemantics .
            tupleSemantics .
            stringSemantics .
            asciiSemantics .
            natSemantics .
            boolSemantics
