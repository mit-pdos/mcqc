{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
module Boogie.Z3.Solution 
    ( solveConstr
    , extract
    , SolveResult (..)
    ) where

import           Control.Applicative
import           Control.Lens ((%=), uses, use)
import           Control.Monad

import           Data.Generics (everything, mkQ, gmapQ)
import           Data.List (intercalate)
import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Data.Map as Map
import           Data.Map (Map)

import           Z3.Monad

import           Boogie.AST
import           Boogie.Position
import           Boogie.PrettyAST ()
import           Boogie.Solver
import           Boogie.Z3.Eval
import           Boogie.Z3.GenMonad
import           Boogie.Z3.Minimize

-- | Update the state's reference map with the references in the
-- supplied expressions. This requires that the sorts already be
-- in place in the state.
updateRefMap :: [Expression] -> Z3Gen ()
updateRefMap = mapM_ addRefs
    where
      addRefs :: Expression -> Z3Gen ()
      addRefs e =
          do let rs = refs e
             pairs <- mapM (\r -> (r,) <$> declareRef r) (Set.toList rs)
             let updMap = Map.fromList pairs
             refMap %= Map.union updMap

      -- | Get the values from a single expression.
      refs :: Expression -> Set TaggedRef
      refs expr = refE expr -- valueT expr `Set.union` exprT expr
          where
            refE e =
                case node e of
                  Literal v -> valueRef v
                  Var _ -> Set.empty
                  Logical t ref -> Set.singleton (LogicRef t ref)
                  Application _ es -> Set.unions (map refE es)
                  MapSelection e es -> Set.unions (map refE (e:es))
                  MapUpdate e es e' -> Set.unions (map refE (e:e':es))
                  Old e -> refE e
                  IfExpr e1 e2 e3 -> Set.unions (map refE [e1, e2, e3])
                  Coercion e _ -> refE e
                  UnaryExpression _ e -> refE e
                  BinaryExpression _ e1 e2 -> Set.unions (map refE [e1, e2])
                  Quantified _ _ _ e -> refE e

            valueRef (Reference t r) = Set.singleton (MapRef t r)
            valueRef _ = Set.empty

      refStr :: TaggedRef -> String
      refStr (LogicRef _ r) = "logical_" ++ show r
      refStr (MapRef t r)   = intercalate "_" ["map", show r, typeString t]

      refType :: TaggedRef -> Type
      refType (LogicRef t _) = t
      refType (MapRef t _)   = t

      declareRef :: TaggedRef -> Z3Gen AST
      declareRef tRef =
          do symbol <- mkStringSymbol (refStr tRef)
             sort   <- lookupSort (refType tRef)
             mkConst symbol sort

-- | Constrains the values in the custom types so they are all
-- non-negative.
customConstrs :: Z3Gen ()
customConstrs =
    do assocs <- Map.toList <$> use refMap
       mapM_ go assocs
    where      
      go (LogicRef t ref, ast) = goType t ast
      go _ = return ()

      goType t@(IdType ident types) ast =
          do proj <- lookupCustomProj ident types
             v <- mkApp proj [ast]
             zero <- mkIntNum 0
             gt <- mkGe v zero
             assert gt
      goType _ _ = return ()

data SolveResult
    = NoSoln
    | Soln
    | SolnWithModel Solution
  deriving Show

-- | Given a set of constraint expressions produce a mapping
-- of references to their concrete values.
--
-- The constraint expressions will have no regular variables,
-- only logical variables and map variables.
solveConstr :: Bool -> Bool -> [Expression] -> Z3Gen SolveResult
solveConstr minWanted solnWanted constrs = 
    do updateRefMap constrs
       debug ("solveConstr: finished map updates")
       mapM_ (evalExpr >=> assert) constrs
       customConstrs
       dummyPreds
       debug ("solveConstr: asserting constraints")
       if solnWanted
         then
           do (_result, modelMb) <- getModel
              debug ("solveConstr: minimizing: " ++ show minWanted)
              case modelMb of
                Just model ->
                  do m <- if minWanted
                          then minimizeModel model
                          else return model
                     SolnWithModel <$> reconstruct m
                Nothing -> return NoSoln
         else
           do result <- check
              case result of
                Unsat -> return NoSoln
                _ -> return Soln


-- | Generate dummy predicates to force values for everything
-- we want in the model.
dummyPreds :: Z3Gen ()
dummyPreds =
  do pBoolStr <- mkStringSymbol "dummy_p_bool"
     pIntStr <- mkStringSymbol "dummy_p_int"
     bSort <- mkBoolSort
     iSort <- mkIntSort
     
     pBool <- mkFuncDecl pBoolStr [bSort] bSort
     pInt <- mkFuncDecl pIntStr [iSort] bSort
     
     assocs <- uses refMap Map.toList
     
     let go (LogicRef t _ref, ast) =
           case t of
             BoolType -> mkApp pBool [ast] >>= assert
             IntType -> mkApp pInt [ast] >>= assert
             IdType ident types ->
                 do (_, _, proj) <- lookupCustomType ident types
                    inner <- mkApp proj [ast]
                    mkApp pInt [inner] >>= assert
         go _ = return ()
     
     mapM_ go assocs
     
     return ()

-- | Extracts a particular type from an AST node, evaluating
-- the node first.
extract :: Model -> Ref -> Type -> AST -> Z3Gen Value
extract model ref t ast = 
    do Just ast' <- eval model ast
       str <- astToString ast
       str' <- astToString ast'
       debug (unwords ["extract:", str, str', show t])
       v <- case t of 
         IntType -> IntValue <$> getInt ast'
         BoolType -> 
             do bMb <- getBoolValue ast'
                case bMb of
                  Just b -> return $ BoolValue b
                  Nothing -> return $ BoolValue False
                      -- error $ unwords ["solveConstr.reconstruct.extract:"
                      --                 ,"couldn't extract bool from logical"
                      --                 ,show ref
                      --                 ]
         IdType ident types ->
             do proj <- lookupCustomProj ident types
                extr <- mkApp proj [ast']
                Just evald <- eval model extr
                int <- getInt evald
                return (CustomValue t $ fromIntegral int)
         _ ->
             error $ concat [ "solveConstr.reconstruct.extract: can't "
                            , "extract maptypes like "
                            , show t
                            ]
       debug (unwords ["extract:", show v])
       return v
-- | From a model and a mapping of values to Z3 AST nodes reconstruct
-- a mapping from references to values. This extracts the appropriate
-- values from the model.
reconstruct :: Model -> Z3Gen Solution
reconstruct model =
    do debug ("reconstruct: start")
       logicMap <- reconMaps
       debug ("reconstruct: end")
       return logicMap
    where
      extract' = extract model

      -- | Reconstruct all maps
      reconMaps :: Z3Gen (Map Ref Value)
      reconMaps = 
          do refAssoc <- uses refMap Map.toList 
             foldM go Map.empty refAssoc
          where go m (tRef, ast) =
                    case tRef of
                      LogicRef t ref -> 
                          do(r, v) <- reconLogicRef t ref ast
                            return (Map.insert r v m)
                      _ -> return m

      -- | Reconstruct a ref/value pair for a logical reference.
      reconLogicRef :: Type -> Ref -> AST -> Z3Gen (Ref, Value)
      reconLogicRef t ref ast =
          do debug ("reconLogicRef: start")
             Just ast' <- eval model ast
             x <- extract' ref t ast'
             debug ("reconLogicRef: end")
             return (ref, x)
