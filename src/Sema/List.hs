{-# LANGUAGE RecordWildCards, OverloadedStrings  #-}
module Sema.List where
import Common.Flatten
import CIR.Expr
import Data.Maybe

-- Deconstructs a list to it's static part and dynamic part, in order to append them
deconstruct :: CExpr -> ([CExpr], Maybe CExpr)
-- deconstruct d | trace ("DBG Sema/List.hs/deconstruct " ++ show d) False = undefined
deconstruct CExprCall { _fname = "Datatypes.Coq_cons", _fparams = [a, b] } =
    case deconstruct b of
        (ls, Just bb) -> (a:ls, snd . deconstruct $ bb)
        (ls, Nothing) -> (a:ls, Nothing)
deconstruct CExprCall { _fname = "Datatypes.Coq_nil", _fparams = [] } =
    ([], Nothing)
deconstruct CExprCall { _fname = "Datatypes.Coq_cons", _fparams = _ } =
    error "Non-binary Coq_cons found in source, undefined behavior"
deconstruct CExprCall { _fname = "Datatypes.Coq_nil", _fparams = _ } =
    error "Non-nullary Coq_nil found in source, undefined behavior"
deconstruct other = ([], Just other)

-- List semantics, ie:
--   * cons(1, cons(2, cons(3, []))) = [1,2,3]
--   * cons(1, cons(add 2 3, []) = [1, 2 + 3]
--   * foo :: Nat -> List Nat
--   * cons(1, cons(2, foo n)) = [1, 2] ++ foo n
listSemantics :: CExpr -> CExpr
listSemantics c = case deconstruct c of
    -- All of list unrolled successfully
    (l, Nothing) -> CExprList $ map listSemantics l
    -- Some expression remained, do not use append haphazardly
    ([a], Just e) -> CExprCall "cons" [listSemantics a, descend listSemantics e]
    ([], Just e) -> descend listSemantics e
    (l, Just e) -> CExprCall "append" [CExprList $ map listSemantics l, descend listSemantics e]

