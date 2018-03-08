{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
module Boogie.Z3.Solver (getEnv, solve, solver) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Concurrent
import           Control.Exception

import           Data.Foldable (Foldable, toList)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.Set as Set
import           Data.List

import           System.IO.Unsafe

import qualified Z3.Base as Z3 (mkConfig, mkContext, mkSolver, mkSimpleSolver)
import           Z3.Monad hiding (Context, Solver)
import qualified Z3.Monad as Z3 (Z3Env)

import           Boogie.AST
import           Boogie.Generator
import           Boogie.Position
import           Boogie.Solver
import           Boogie.TypeChecker
import           Boogie.Util ((|=|), conjunction, enot)
import           Boogie.Z3.GenMonad
import           Boogie.Z3.Solution

solver :: (MonadPlus m, Foldable m)
      => Bool          -- ^ Is a minimal solution desired?
      -> Maybe Int     -- ^ Bound on number of solutions
      -> Solver m
solver minWanted mBound = unsafePerformIO $ mkSolver minWanted mBound

mkSolver :: (MonadPlus m, Foldable m)
      => Bool          -- ^ Is a minimal solution desired?
      -> Maybe Int     -- ^ Bound on number of solutions
      -> IO (Solver m)
mkSolver minWanted mBound = do
    envNoModel <- getEnv False
    envModel <- getEnv True
    return Solver {
          solPick = \cs state -> do 
            (mSolution, newNAssert) <- solve minWanted True mBound cs (pickState state) envModel
            case mSolution of
              NoSoln -> mzero
              Soln -> error "solution found, but no model requested"
              SolnWithModel solution -> return (solution, state { pickState = newNAssert }),
          solCheck = \cs state ->
                      let (mSolution, newNAssert) = head $ solve False False (Just 1) cs (checkState state) envNoModel
                          foundSoln = case mSolution of
                                        NoSoln -> False
                                        _ -> True
                      in (foundSoln, state { checkState = newNAssert })
        }

getEnv :: Bool -> IO (Z3.Z3Env)
getEnv modelWanted = newEnv (Just AUFLIA) opts
    where
      opts = stdOpts -- +? (opt "auto_config" False)
                     -- +? (opt "model" modelWanted)
                     -- +? (opt "MBQI" False)
                     -- +? (opt "SOFT_TIMEOUT" (100::Int))
                     -- +? (opt "MODEL_ON_TIMEOUT" True)

solve :: (MonadPlus m, Foldable m)
      => Bool          -- ^ Is a minimal solution desired?
      -> Bool          -- ^ Is a solution wanted?
      -> Maybe Int     -- ^ Bound on number of solutions
      -> ConstraintSet -- ^ Set of constraints
      -> Int           -- ^ Desired number of backtracking points in the solver
      -> Z3.Z3Env      -- ^ Z3 solver and context to use
      -> m (SolveResult, Int)
solve minWanted solnWanted mBound constrs nAssert env = 
    case solRes of
      SolnWithModel soln -> return (solRes, newNAssert) `mplus` go
          where
            neq = newConstraint soln
            go = if mBound == Nothing || (fromJust mBound > 1)
                    then solve
                           minWanted
                           solnWanted
                           (fmap pred mBound)
                           (neq : constrs)
                           nAssert
                           env
                    else mzero
      _ -> return x
  where
    x@(solRes, newNAssert) =
      stepConstrs minWanted solnWanted constrs nAssert env
data StepResult
    = StepNoSoln
    | StepSoln
    | StepSolnWithModel Solution Expression

stepConstrs :: Bool
            -> Bool
            -> [Expression]
            -> Int
            -> Z3.Z3Env
            -> (SolveResult, Int)
stepConstrs minWanted solnWanted constrs nAssert env = unsafePerformIO act
    where
      act = 
       do evalZ3GenWith env $ 
           do 
              debug ("stepConstrs: start")
              debug ("stepConstrs: " ++ show (minWanted, constrs, nAssert))
              debug1 ("interpreter thinks " ++ show nAssert)              
              popStack
              push
              debug1 ("constraints " ++ show (length constrs) ++ "\n" ++ (intercalate "\n" $ map show constrs))              
              solnRes <- solveConstr minWanted solnWanted constrs
              debug1 (show solnRes)
              newNAssert <- getNumScopes
              debug ("new " ++ show newNAssert) 
              debug ("stepConstrs: done")
              return (solnRes, newNAssert)
      popStack = do
        nAssertSolver <- getNumScopes
        debug1 ("solver thinks " ++ show nAssertSolver)
        if nAssert == 0
          then reset
          else if nAssert > nAssertSolver
            then error "Solver has fewer assertions than the interpreter"
            else if nAssert < nAssertSolver
              then do
                debug ("pop")
                pop 1
                popStack
              else return ()

newConstraint :: Solution -> Expression
newConstraint soln = enot (conjunction (logicEqs ++ customEqs))
    where
      logicEq :: Ref -> Expression -> Expression
      logicEq r e = logic e r |=| e
      
      -- Logical equations only for non-idType values.
      logicEqs :: [Expression]
      logicEqs = Map.foldrWithKey go [] soln
          where
            go ref expr es =
                case valueType expr of
                  t@(IdType _ _) -> es
                  _ -> logicEq ref (gen (Literal expr)) : es

      logict t r = gen (Logical t r)
      logic e r = gen (Logical (thunkType e) r)

      customEqs :: [Expression]
      customEqs = eqs ++ notEqs
          where
            eqs = concatMap (uncurry eqFold) (Map.toList customEqRel)
            notEqs = concat $ map snd $
                     Map.toList $ Map.mapWithKey allNeqs neqMaps
                where
                  neq t e r = enot (e |=| logict t r)
                  neqs t e = map (neq t e)

                  allNeqs :: Type -> [Ref] -> [Expression]
                  allNeqs t [] = []
                  allNeqs t (r:rs) = neqs t (logict t r) rs ++ allNeqs t rs

                  neqMaps :: Map Type [Ref]
                  neqMaps = Map.mapKeysWith (++) thunkType
                              (Map.map mkNeqData customEqRel)
                  mkNeqData refs = [head $ Set.toList refs]

            eqOp e r1 r2  = logic e r1 |=| logic e r2
            neqOp e r1 r2 = enot (eqOp e r1 r2)

            interPair op e [r1]       = [op e r1 r1]
            interPair op e (r1:r2:rs) = (op e r1 r2):(interPair op e (r2:rs))

            eqFold expr = interPair eqOp expr . Set.toList
            neqFold expr = interPair neqOp expr

      -- Equality relation on customs.
      customEqRel = Map.foldWithKey go Map.empty soln
          where
            go ref expr m =
                case valueType expr of
                  t@(IdType _ _) -> 
                      Map.insertWith Set.union (gen (Literal expr)) (Set.singleton ref) m
                  _ -> m
