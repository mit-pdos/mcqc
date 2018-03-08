-- | Various normal forms of Boolean expressions
module Boogie.NormalForm where

import Boogie.AST
import Boogie.Position
import Boogie.Util
import Boogie.TypeChecker
import Data.Map (Map, (!))
import qualified Data.Map as M

-- | Negation normal form of a Boolean expression:
-- no negation above boolean connectives, quantifiers or relational operators;
-- no boolean connectives except @&&@ and @||@
negationNF :: Expression -> Expression
negationNF boolExpr = case node boolExpr of
  UnaryExpression Not e -> case node e of
    UnaryExpression Not e' -> negationNF e'
    BinaryExpression And e1 e2 -> negationNF (enot e1) ||| negationNF (enot e2)
    BinaryExpression Or e1 e2 -> negationNF (enot e1) |&| negationNF (enot e2)
    BinaryExpression Implies e1 e2 -> negationNF e1 |&| negationNF (enot e2)
    BinaryExpression Explies e1 e2 -> negationNF (enot e1) |&| negationNF e2
    BinaryExpression Equiv e1 e2 -> (negationNF e1 |&| negationNF (enot e2)) |&| (negationNF (enot e1) |&| negationNF e2)
    BinaryExpression Eq e1 e2 -> e1 |!=| e2
    BinaryExpression Neq e1 e2 -> e1 |=| e2
    BinaryExpression Leq ae1 ae2 -> ae1 |>| ae2
    BinaryExpression Ls ae1 ae2 -> ae1 |>=| ae2
    BinaryExpression Geq ae1 ae2 -> ae1 |<| ae2
    BinaryExpression Gt ae1 ae2 -> ae1 |<=| ae2
    Quantified Forall tv vars e' -> attachPos (position e) $ Quantified Exists tv vars (negationNF (enot e'))
    Quantified Exists tv vars e' -> attachPos (position e) $ Quantified Forall tv vars (negationNF (enot e'))
    _ -> boolExpr
  BinaryExpression Implies e1 e2 -> negationNF (enot e1) ||| negationNF e2
  BinaryExpression Explies e1 e2 -> negationNF e1 ||| negationNF (enot e2)
  BinaryExpression Equiv e1 e2 -> (negationNF (enot e1) ||| negationNF e2) |&| (negationNF e1 ||| negationNF (enot e2))
  BinaryExpression op e1 e2 
    | op == And || op == Or -> inheritPos2 (BinaryExpression op) (negationNF e1) (negationNF e2)
    | otherwise -> case node e1 of -- relational operator
      IfExpr cond e11 e12 -> negationNF ((cond |=>| inheritPos2 (BinaryExpression op) e11 e2) |&| (enot cond |=>| inheritPos2 (BinaryExpression op) e12 e2))
      _ -> case node e2 of
        IfExpr cond e21 e22 -> negationNF ((cond |=>| inheritPos2 (BinaryExpression op) e1 e21) |&| (enot cond |=>| inheritPos2 (BinaryExpression op) e1 e22))
        _ -> boolExpr
  IfExpr cond e1 e2 -> negationNF ((cond |=>| e1) |&| (enot cond |=>| e2))
  Quantified qop tv vars e -> attachPos (position boolExpr) $ Quantified qop tv vars (negationNF e)
  _ -> boolExpr

-- | Prenex normal form of a Boolean expression:
-- all quantifiers are pushed to the outside and any two quantifiers of the same kind in a row are glued together.
-- Requires expression to be in the negation normal form.  
prenexNF :: Expression -> Expression
prenexNF boolExpr = glue $ rawPrenex boolExpr
  where
    -- | Push all quantifiers to the front
    rawPrenex boolExpr = case node boolExpr of
      -- We only have to consider && and || because boolExpr is in negation normal form
      BinaryExpression op e1 e2 | op == And || op == Or -> merge (++ "1") (++ "2") op (rawPrenex e1) (rawPrenex e2)
      Quantified qop tv vars e -> attachPos (position boolExpr) $ Quantified qop tv vars (rawPrenex e)
      _ -> boolExpr
    merge r1 r2 op e1 e2 = attachPos (position e1) (merge' r1 r2 op e1 e2)
    merge' r1 r2 op (Pos _ be1@(Quantified _ _ _ _)) e2 = let Quantified qop tv' vars' e' = renameBound r1 be1
      in Quantified qop tv' vars' (merge r1 r2 op e' e2)
    merge' r1 r2 op e1 (Pos _ be2@(Quantified _ _ _ _)) = let Quantified qop tv' vars' e' = renameBound r2 be2
      in Quantified qop tv' vars' (merge r1 r2 op e1 e')
    merge' _ _ op e1 e2 = BinaryExpression op e1 e2
    -- | Rename all bound variables and type variables in a quantified expression with a renaming function r
    renameBound r (Quantified qop tv vars e) = Quantified qop (map r tv) (map (renameVar r tv) vars) (exprSubst (varBinding r (map fst vars)) e)
    varBinding r ids = M.fromList $ zip ids (map (gen . Var . r) ids)
    typeBinding r tv = M.fromList $ zip tv (map (nullaryType . r) tv)
    renameVar r tv (id, t) = (r id, typeSubst (typeBinding r tv) t)
    -- | Glue together any two quantifiers of the same kind in a row
    glue boolExpr = attachPos (position boolExpr) (glue' (node boolExpr))
    glue' boolExpr = case boolExpr of
      Quantified qop tv vars e -> case node e of
        Quantified qop' tv' vars' e' | qop == qop' -> glue' (Quantified qop (tv ++ tv') (vars ++ vars') e')
                                     | otherwise -> Quantified qop tv vars (glue e)
        _ -> boolExpr
      _ -> boolExpr

-- | Negation and prenex normal form of a Boolean expression
normalize :: Expression -> Expression      
normalize boolExpr = prenexNF $ negationNF boolExpr
