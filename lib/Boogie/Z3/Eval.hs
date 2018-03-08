module Boogie.Z3.Eval (evalExpr) where

import           Control.Applicative
import           Control.Lens (uses)
import           Control.Monad

import           Data.Map (Map)
import qualified Data.Map as Map

import           Z3.Monad hiding (Context)
import qualified Z3.Monad as Z3 (Context)

import           Boogie.AST
import           Boogie.Position
import           Boogie.PrettyAST ()
import           Boogie.TypeChecker
import           Boogie.Z3.GenMonad

evalExpr = evalExpr' Map.empty emptyContext

-- | Evaluate an expression to a Z3 AST.
evalExpr' :: Map String AST -- ^ Map of bound variables.
          -> Context        -- ^ Type context of bound variables.
          -> Expression     -- ^ Expression to evaluate.
          -> Z3Gen AST
evalExpr' boundMap typeCtx expr = debug ("evalExpr': " ++ show expr) >>
    case node expr of
      Var ident ->
          case Map.lookup ident boundMap of
            Just ast -> return ast
            Nothing -> error $ concat $
                       ["evalExpr': didin't find ", ident,
                        " in ", show boundMap]
      Literal v -> evalValue v
      Logical t ref -> uses refMap (lookup' "evalExpr'" (LogicRef t ref))
      MapSelection m args ->
          do m' <- go m
             arg <- tupleArg args
             mkSelect m' arg
      MapUpdate m args val ->
          do m' <- go m
             arg <- tupleArg args
             val' <- go val
             mkStore m' arg val'
      UnaryExpression op e -> go e >>= unOp op
      BinaryExpression op e1 e2 -> join (binOp op <$> go e1 <*> go e2)
      IfExpr c e1 e2 -> join (mkIte <$> go c <*> go e1 <*> go e2)
      Quantified qop ids idTypes e ->
          do let (names, types) = unzip idTypes
             symbs <- mapM mkStringSymbol names
             sorts <- mapM lookupSort types
             consts <- zipWithM mkConst symbs sorts
             let boundMap' = Map.union boundMap (Map.fromList (zip names consts))
                 typeCtx'  = nestedContext Map.empty idTypes typeCtx
             apps <- mapM toApp consts
             debug ("evalExpr': after toApp")
             e' <- evalExpr' boundMap' typeCtx' e
             ast <- quant qop [] apps e'
             debug ("evalExpr': after quant")
             return ast
      e -> error $ "solveConstr.evalExpr': " ++ show e
    where
      evalValue :: Value -> Z3Gen AST
      evalValue v =
          case v of
            IntValue i      -> mkIntNum i
            BoolValue True  -> mkTrue
            BoolValue False -> mkFalse
            Reference t ref -> uses refMap (lookup' "evalValue" (MapRef t ref))
            CustomValue (IdType ident types) ref ->
                do ctor <- lookupCustomCtor ident types
                   refAst <- mkIntNum ref
                   mkApp ctor [refAst]
            _ -> error $ "evalValue: can't handle value: " ++ show v

      go e = evalExpr' boundMap typeCtx e

      quant Forall = mkForallConst

      tupleArg :: [Expression] -> Z3Gen AST
      tupleArg es =
          do let ts = map (exprType typeCtx) es
             debug (show ts)
             (_sort, ctor, _projs) <- lookupCtor ts
             es' <- mapM go es
             c <- mkApp ctor es'
             return c

      unOp :: UnOp -> AST -> Z3Gen AST
      unOp Neg = mkUnaryMinus
      unOp Not = mkNot

      binOp :: BinOp -> AST -> AST -> Z3Gen AST
      binOp op =
          case op of
            Eq -> mkEq
            Gt -> mkGt
            Ls -> mkLt
            Leq -> mkLe
            Geq -> mkGe
            Neq -> \ x y -> mkEq x y >>= mkNot

            Plus -> list2 mkAdd
            Minus -> list2 mkSub
            Times -> list2 mkMul
            Div   -> mkDiv
            Mod   -> mkMod

            And   -> list2 mkAnd
            Or    -> list2 mkOr
            Implies -> mkImplies
            Equiv -> mkIff
            Explies -> flip mkImplies
            Lc -> error "solveConstr.binOp: Lc not implemented"
          where list2 o x y = o [x, y]

