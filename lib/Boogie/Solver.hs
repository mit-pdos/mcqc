{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

-- | Constraint solver interface
module Boogie.Solver where

import Boogie.AST
import Boogie.Pretty
import Boogie.PrettyAST
import Data.Map (Map, (!))

import Z3.Monad as Z3 (Context)

-- | Set of constraints
type ConstraintSet = [Expression]

constraintSetDoc :: ConstraintSet -> Doc
constraintSetDoc = vsep . map pretty

-- | Mapping from logical variables to their values
type Solution = Map Ref Value

instance Pretty Solution where
  pretty = vMapDoc logDoc pretty
  
-- | Solver state for incremental solving  
data SolverState = SolverState {
  checkState :: Int,    -- ^ State of the checking solver
  pickState :: Int      -- ^ State of the picking solver
}

-- | Initial solver state
initSolverState :: SolverState
initSolverState = SolverState 0 0

-- | Solver: produces solutions of constraint sets
data Solver m = Solver {
  solCheck :: ConstraintSet -> SolverState -> (Bool, SolverState),      -- ^ Given a constraint set and a current solver state id, return whether the constraint set is satisfiable and the new state id
  solPick :: ConstraintSet -> SolverState -> m (Solution, SolverState)  -- ^ Given a constraint set and a current solver state id, return solution(s) and the new state id
}
