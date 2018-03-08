{-# LANGUAGE TemplateHaskell, Rank2Types, FlexibleInstances, TypeSynonymInstances #-}

-- | Execution state for the interpreter
module Boogie.Environment ( 
  Store,
  emptyStore,
  prettyFlatThunk,
  prettyFlatStore,
  MapInstance,
  MapCache,
  MapPointKind (..),
  MapAux,
  MapAuxCache,  
  Memory,
  StoreLens,
  memLocals,
  memGlobals,
  memOld,
  memModified,
  memConstants,
  memMaps,
  memMapsAux,
  memLogical,
  memOutput,
  emptyMemory,
  visibleVariables,
  userMemory,
  memoryDoc,
  NameConstraints,
  MapConstraints,
  constraintUnion,  
  ConstraintMemory,
  conLocals,
  conGlobals,
  conUnique,
  conMaps,
  conPointQueue,
  conLogical,
  conLogicalChecked,
  Environment,
  envMemory,
  envProcedures,
  envFunctions,
  envConstraints,
  envTypeContext,
  envSolver,
  envSolverState,
  envGenerator,
  envMapCount,
  envLogicalCount,
  envInOld,
  envLabelCount,
  envMapCaseCount,
  envRecMax,
  envLoopMax,
  envConcretize,
  initEnv,
  lookupProcedure,
  lookupNameConstraints,
  lookupUnique,
  lookupMapConstraints,
  addProcedureImpl,
  addNameConstraint,
  addUniqueConst,
  addMapConstraint,
  addLogicalConstraint,
  markModified,
  isRecursive
) where

import Boogie.Util
import Boogie.Position
import Boogie.AST
import Boogie.Solver
import Boogie.Generator
import Boogie.TypeChecker (Context, ctxGlobals)
import Boogie.Pretty
import Boogie.PrettyAST
import Data.Maybe
import Data.Foldable (toList)
import Data.List
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Sequence (Seq, (|>), viewl)
import qualified Data.Sequence as Seq
import Control.Lens hiding (Context, at)
  
{- Memory -}

-- | Store: stores variable values at runtime 
type Store = Map Id Thunk

-- | A store with no variables
emptyStore = M.empty

-- | Pretty-printed store
instance Pretty Store where
  pretty = vMapDoc pretty pretty
  
-- | Pretty-print a store, inlining all map references  
prettyFlatStore :: MapCache -> Store -> Doc
prettyFlatStore maps = vMapDoc pretty (prettyFlatThunk maps)
    
-- | Partial map instance
type MapInstance = Map [Thunk] Thunk

instance Pretty MapInstance where
  pretty = let keysDoc keys = ((if length keys > 1 then parens else id) . commaSep . map pretty) keys
    in hMapDoc keysDoc pretty
    
-- | MapCache: stores partial map instances    
type MapCache = Map Ref MapInstance
    
emptyCache = M.empty  
  
instance Pretty MapCache where
  pretty = vMapDoc refDoc pretty
  
-- | Pretty-print a thunk, inlining all map references  
prettyFlatThunk :: MapCache -> Thunk -> Doc
prettyFlatThunk maps (Pos _ (Literal (Reference t r))) = 
  let keysDoc keys = ((if length keys > 1 then parens else id) . commaSep . map (prettyFlatThunk maps)) keys
  in hMapDoc keysDoc (prettyFlatThunk maps) (maps ! r)
prettyFlatThunk maps thunk = pretty thunk  
  
-- | Uniqueness of a point stored in map cache  
data MapPointKind = 
  UniquePoint |     -- ^ Unique map point (used to instantiate map constraints)
  DuplicatePoint |  -- ^ Map point that is equal to some unique map point
  UnknownPoint      -- ^ Yet to be decided if this point is unique or duplicate
    deriving (Eq, Show)

-- | For each map point, stores the order of its insertion into the instance and its uniqueness
type MapAux = Map [Thunk] (Int, MapPointKind)

instance Pretty MapAux where
  pretty = let keysDoc keys = ((if length keys > 1 then parens else id) . commaSep . map pretty) keys
    in hMapDoc keysDoc (\(idx, kind) -> commaSep [int idx, text (show kind)])

-- | Stores auxiliary information about map instances useful when instantiating map constraints
type MapAuxCache = Map Ref MapAux  

instance Pretty MapAuxCache where
  pretty = vMapDoc refDoc pretty

-- | Memory: stores thunks associated with names, map references and logical variables
data Memory = Memory {
  _memLocals :: Store,        -- ^ Local variable store
  _memGlobals :: Store,       -- ^ Global variable store
  _memOld :: Store,           -- ^ Old global variable store (in two-state contexts)
  _memModified :: Set Id,     -- ^ Set of global variables, which have been modified since the beginning of the current procedure  
  _memConstants :: Store,     -- ^ Constant store  
  _memMaps :: MapCache,       -- ^ Partial instances of maps
  _memMapsAux :: MapAuxCache, -- ^ Auxiliary information about map points
  _memLogical :: Solution,    -- ^ Logical variable store
  _memOutput :: [[AttrValue]] -- ^ List of lines of output performed by the program
} deriving Eq

makeLenses ''Memory

-- | Lens that selects a store from memory
type StoreLens = Simple Lens Memory Store

-- | Empty memory
emptyMemory = Memory {
  _memLocals = emptyStore,
  _memGlobals = emptyStore,
  _memOld = emptyStore,
  _memModified = S.empty,
  _memConstants = emptyStore,
  _memMaps = emptyCache,
  _memMapsAux = M.empty,
  _memLogical = M.empty,
  _memOutput = []
}

-- | Visible values of all identifiers in a memory (locals shadow globals) 
visibleVariables :: Memory -> Store
visibleVariables mem = (mem^.memLocals) `M.union` (mem^.memGlobals) `M.union` (mem^.memConstants)

-- | Map references directly reachable from the store
storedRefs :: Memory -> Set Ref
storedRefs mem = S.fromList $ storedIn (mem^.memLocals) ++ 
                              storedIn (mem^.memGlobals) ++ 
                              storedIn (mem^.memConstants) ++ 
                              storedIn (mem^.memOld) ++
                              storedInOutput
  where
    storedIn store = catMaybes . map (toMapRef . node) . M.elems $ store    
    toMapRef (Literal (Reference _ r)) = Just r
    toMapRef _ = Nothing
    storedInOutput = catMaybes . map attrToMapRef . concat $ (mem^.memOutput)
    attrToMapRef (EAttr (Pos _ ((Literal (Reference _ r))))) = Just r
    attrToMapRef _ = Nothing

-- -- | 'userStore' @conMem mem@ : @mem@ with all reference values completely dereferenced and cache of defined maps removed 
userMemory :: ConstraintMemory -> Memory -> Memory
userMemory conMem mem = let maps = mem^.memMaps in
  over memModified (const S.empty) $
  over memMapsAux (const M.empty) $
  over memLogical (const M.empty)
  mem

-- | 'memoryDoc' @inNames outNames flatten mem@ : pretty-printed @mem@ where
-- locals in @inNames@ will be printed as input variables
-- and locals in @outNames@ will be printed as output variables
memoryDoc :: [Id] -> [Id] -> Bool -> Memory -> Doc
memoryDoc inNames outNames flatten mem = vsep $ 
  docNonEmpty ins (labeledStoreDoc "Ins") ++
  docNonEmpty locals (labeledStoreDoc "Locals") ++
  docNonEmpty outs (labeledStoreDoc "Outs") ++
  docNonEmpty allGlobals (labeledStoreDoc "Globals") ++
  docNonEmpty (mem^.memOld) (labeledStoreDoc "Old globals") ++
  docWhen (not (S.null $ mem^.memModified)) (text "Modified:" <+> commaSep (map text (S.toList $ mem^.memModified))) ++
  docWhen (not flatten && not (M.null (mem^.memMaps))) (labeledDoc "Maps" (mem^.memMaps)) ++
  docNonEmpty (mem^.memMapsAux) (labeledDoc "MapAux") ++
  docNonEmpty (mem^.memLogical) (labeledDoc "Logical")
  where
    allLocals = mem^.memLocals
    ins = restrictDomain (S.fromList inNames) allLocals
    outs = restrictDomain (S.fromList outNames) allLocals
    locals = removeDomain (S.fromList $ inNames ++ outNames) allLocals
    allGlobals = (mem^.memGlobals) `M.union` (mem^.memConstants)
    labeledDoc label x = (text label <> text ":") <+> align (pretty x)
    labeledStoreDoc label x = (text label <> text ":") <+> align (if flatten then prettyFlatStore (mem^.memMaps) x else pretty x)
    docWhen flag doc = if flag then [doc] else [] 
    docNonEmpty m mDoc = docWhen (not (M.null m)) (mDoc m)
    
instance Pretty Memory where
  pretty mem = memoryDoc [] [] False mem
  
{- Constraint memory -}

-- | Mapping from names to their constraints
type NameConstraints = Map Id ConstraintSet

-- | Pretty-printed variable constraints
instance Pretty NameConstraints where
  pretty = vMapDoc pretty constraintSetDoc

-- | Mapping from map references to their parametrized constraints
type MapConstraints = Map Ref ConstraintSet

instance Pretty MapConstraints where
  pretty = vMapDoc refDoc constraintSetDoc  
  
-- | Union of constraints (values at the same key are concatenated)
constraintUnion s1 s2 = M.unionWith (++) s1 s2  

-- | Sequence of map points (ref-args pairs)
type PointQueue = Seq (Ref, [Thunk])

instance Pretty PointQueue where
  pretty queue = let pointDoc (r, args) = refDoc r <> brackets (commaSep (map pretty args))
    in commaSep (map pointDoc $ toList queue)

-- | Constraint memory: stores constraints associated with names, map references and logical variables
data ConstraintMemory = ConstraintMemory {
  _conLocals :: NameConstraints,        -- ^ Local name constraints
  _conGlobals :: NameConstraints,       -- ^ Global name constraints
  _conUnique :: Map Type [Id],          -- ^ For each type, the list of constants of that type declared unique
  _conMaps :: MapConstraints,           -- ^ Parametrized map constraints
  _conPointQueue :: PointQueue,         -- ^ Map points that have been accessed, but not constrained yet 
  _conLogical :: ConstraintSet,         -- ^ Constraint on logical variables
  _conLogicalChecked :: ConstraintSet   -- ^ Constraint on logical variables already checked and stored by the solver
}

makeLenses ''ConstraintMemory

-- | Symbolic memory with no constraints
emptyConstraintMemory = ConstraintMemory {
  _conLocals = M.empty,
  _conGlobals = M.empty,
  _conUnique = M.empty,
  _conMaps = M.empty,
  _conPointQueue = Seq.empty,
  _conLogical = [],
  _conLogicalChecked = []
}

constraintMemoryDoc :: ConstraintMemory -> Doc
constraintMemoryDoc mem = vsep $ 
  docNonEmpty (mem^.conLocals) (labeledDoc "CLocal") ++
  docNonEmpty (mem^.conGlobals) (labeledDoc "CGlobal") ++
  docNonEmpty (mem^.conMaps) (labeledDoc "CMap") ++
  docWhen (not $ Seq.null (mem^.conPointQueue)) ((text "CPoints" <> text ":") <+> align (pretty (mem^.conPointQueue))) ++ 
  docWhen (not $ null (mem^.conLogical)) ((text "CLogical" <> text ":") <+> align (constraintSetDoc (mem^.conLogical))) ++
  docWhen (not $ null (mem^.conLogicalChecked)) ((text "CChecked" <> text ":") <+> align (constraintSetDoc (mem^.conLogicalChecked)))
  where
    labeledDoc label x = (text label <> text ":") <+> align (pretty x)
    docWhen flag doc = if flag then [doc] else [] 
    docNonEmpty m mDoc = docWhen (not (M.null m)) (mDoc m)
    
instance Pretty ConstraintMemory where
  pretty = constraintMemoryDoc

{- Environment -}
  
-- | Execution state
data Environment m = Environment
  {
    _envMemory :: Memory,                       -- ^ Values
    _envConstraints :: ConstraintMemory,        -- ^ Constraints
    _envProcedures :: Map Id [PDef],            -- ^ Procedure implementations
    _envFunctions :: Map Id Expression,         -- ^ Functions with definitions
    _envTypeContext :: Context,                 -- ^ Type context
    _envSolver :: Solver m,                     -- ^ Constraint solver
    _envSolverState :: SolverState,             -- ^ An identifier of solver state
    _envGenerator :: Generator m,               -- ^ Value generator
    _envMapCount :: Int,                        -- ^ Number of map references currently in use
    _envLogicalCount :: Int,                    -- ^ Number of logical variables currently in use
    _envLabelCount :: Map (Id, Id) Int,         -- ^ For each procedure-label pair, number of times a transition with that label was taken
    _envMapCaseCount :: Map (Ref, Int) Int,     -- ^ For each guarded map constraint, number of times it was applied
    _envInOld :: Bool,                          -- ^ Is an old expression currently being evaluated?
    _envRecMax :: Maybe Int,                    -- ^ Limit on the number of unfoldings of a recursive constraint
    _envLoopMax :: Maybe Int,                   -- ^ Limit on the number times a label can be taken
    _envConcretize :: Bool                      -- ^ Concretize in the middle of the execution?
  }
  
makeLenses ''Environment
   
-- | 'initEnv' @tc s@: Initial environment in a type context @tc@ with constraint solver @s@  
initEnv tc s g recMax loopMax concr = Environment
  {
    _envMemory = emptyMemory,
    _envConstraints = emptyConstraintMemory,
    _envProcedures = M.empty,
    _envFunctions = M.empty,
    _envTypeContext = tc,
    _envSolver = s,
    _envSolverState = initSolverState,
    _envGenerator = g,
    _envMapCount = 0,
    _envLogicalCount = 0,
    _envLabelCount = M.empty,
    _envMapCaseCount = M.empty,
    _envInOld = False,
    _envRecMax = recMax,
    _envLoopMax = loopMax, 
    _envConcretize = concr
  }
  
-- | 'lookupGetter' @getter def key env@ : lookup @key@ in a map accessible with @getter@ from @env@; if it does not occur return @def@
lookupGetter getter def key env = case M.lookup key (env ^. getter) of
  Nothing -> def
  Just val -> val
  
combineGetters f g1 g2 = to $ \env -> (env ^. g1) `f` (env ^. g2)  
  
-- Environment queries  
lookupProcedure = lookupGetter envProcedures []  
lookupNameConstraints = lookupGetter (combineGetters M.union (envConstraints.conLocals) (envConstraints.conGlobals)) []
lookupUnique = lookupGetter (envConstraints.conUnique) []
lookupMapConstraints = lookupGetter (envConstraints.conMaps) []

-- Environment modifications
addProcedureImpl name def env = over envProcedures (M.insert name (lookupProcedure name env ++ [def])) env
addNameConstraint :: Id -> Simple Lens (Environment m) NameConstraints -> Expression -> Environment m -> Environment m
addNameConstraint name lens c env = over lens (M.insert name (nub $ c : lookupGetter lens [] name env)) env
addUniqueConst t name env = over (envConstraints.conUnique) (M.insert t (name : lookupUnique t env)) env
addMapConstraint r c env = over (envConstraints.conMaps) (M.insert r (nub $ c : lookupMapConstraints r env)) env
addLogicalConstraint c = over (envConstraints.conLogical) (nub . (c :))
markModified name env = if M.member name (env^.envTypeContext.to ctxGlobals) 
  then over (envMemory.memModified) (S.insert name) env
  else env
  
-- | 'isRecursive' @name functions@ : is function @name@ (mutually) recursive according to definitions in @functions@?
isRecursive name functions = name `elem` reachable [] name
  where
    reachable visited f = let direct = called f
      in direct ++ concatMap (reachable (visited ++ direct)) (direct \\ visited)
    called f = case M.lookup f functions of
                      Nothing -> []
                      Just (Pos _ (Quantified Lambda tv vars body)) -> map fst $ applications body
