module Sema.Pipeline where
import CIR.Expr
import Sema.Nat
import Sema.Proc
import Sema.Bool
import Sema.String
import Sema.Pair
import Sema.Option

semantics :: CExpr -> CExpr
semantics = procSemantics .
            pairSemantics .
            optionSemantics .
            stringSemantics .
            asciiSemantics .
            natSemantics .
            boolSemantics
