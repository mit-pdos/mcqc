module Sema.Pipeline where
import CIR.Expr
import Sema.Nat
import Sema.Proc
import Sema.Bool
import Sema.Option
import Sema.List
import Sema.String
import Sema.Tuple

semantics :: CExpr -> CExpr
semantics = procSemantics .
            listSemantics .
            optionSemantics .
            tupleSemantics .
            stringSemantics .
            asciiSemantics .
            natSemantics .
            boolSemantics
