{-# LANGUAGE FlexibleContexts #-}

-- | Type checker for Boogie 2
module Boogie.TypeChecker (
  -- * Checking programs
  typeCheckProgram,
  exprType,
  thunkType,
  resolve,
  TypeError (..),
  typeErrorsDoc,
  -- * Typing context
  Context,
  ctxTypeConstructors,
  ctxGlobals,
  ctxLocals,
  ctxConstants,
  ctxFunctions,
  ctxProcedures,
  ctxTypeVars,
  emptyContext,
  typeNames,
  globalScope,
  localScope,
  mutableVars,
  allVars,
  allNames,
  funProcNames,
  funSig,
  procSig,
  setLocals,
  localContext,
  nestedContext,
  globalContext,
  enterProcedure
) where

import Boogie.AST
import Boogie.Util
import Boogie.ErrorAccum
import Boogie.Position
import Boogie.Pretty
import Data.List
import Data.Maybe
import Data.Map (Map, (!))
import qualified Data.Map as M
import Control.Monad.Trans.Error
import Control.Applicative hiding (empty)
import Control.Monad.State
import Control.Lens hiding (Context)

{- Interface -}

-- | Check program and return type errors if present, and the global typing context otherwise
typeCheckProgram :: Program -> Either [TypeError] Context
typeCheckProgram p = case runState (runErrorT (checkProgram p)) emptyContext of
  (Left errs, _)  -> Left errs
  (_, ctx)        -> Right ctx

-- | 'exprType' @c expr@ :
-- Type of @expr@ in context @c@;
-- fails if expr contains type errors.
exprType :: Context -> Expression -> Type
exprType c expr = case evalState (runErrorT (checkExpression expr)) c of
  Left _ -> (error . show) (text "encountered ill-typed expression during execution:" <+> pretty expr)
  Right t -> t

-- | Type of thunk (does not require type context)
thunkType :: Expression -> Type
thunkType = exprType emptyContext

-- | 'localContext' @inst locals c@ : @c@ with local names replaced by @locals@, thier types instantiated according to @inst@
localContext :: TypeBinding -> [IdType] -> Context -> Context
localContext inst locals c = c { ctxLocals = M.fromList (over (mapped._2) (typeSubst inst) locals) }

-- | 'nestedContext' @inst locals c@ : @c@ with local names extended by @locals@, thier types instantiated according to @inst@ and resolved
nestedContext :: TypeBinding -> [IdType] -> Context -> Context
nestedContext inst locals c = c { ctxLocals = M.fromList (over (mapped._2) (resolve c . typeSubst inst) locals) `M.union` ctxLocals c }

-- | 'globalContext" @c@ : @c@ with local names removed
globalContext :: Context -> Context
globalContext c = c { ctxLocals = M.empty }

-- | 'enterProcedure' @sig def actuals lhss c@ :
-- Local context of procedure @sig@ with definition @def@ and actual arguments @actuals@
-- in a call with left-hand sides @lhss@
enterProcedure :: PSig -> PDef -> [Expression] -> [Expression] -> Context -> Context
enterProcedure sig def actuals lhss c = localContext inst (zip ins (psigArgTypes sig) ++ zip outs (psigRetTypes sig) ++ map noWhere localVars) c
  where
    inst = case evalState (runErrorT (pInstance sig actuals lhss)) c of
      Left _ -> internalError $ text "encountered ill-typed procedure call during execution:" <+>
        text (psigName sig) <+> text "with actual arguments" <+> parens (commaSep (map pretty actuals)) <+>
        text "and left-hand sides" <+> parens (commaSep (map pretty lhss))
      Right u -> u
    localVars = fst (pdefBody def)
    ins = pdefIns def
    outs = pdefOuts def

{- Context -}

-- | Typing context
data Context = Context
  {
    -- Scope context (specific to an AST node, gets restored when type cher leaves the node):
      -- Global:
    ctxTypeConstructors :: Map Id Int,      -- ^ type constructor arity
    ctxTypeSynonyms :: Map Id ([Id], Type), -- ^ type synonym values
    ctxGlobals :: Map Id Type,              -- ^ global variable types (type synonyms resolved)
    ctxConstants :: Map Id Type,            -- ^ constant types (type synonyms resolved)
    ctxFunctions :: Map Id Type,            -- ^ function signatures (type synonyms resolved)
    ctxProcedures :: Map Id PSig,           -- ^ procedure signatures (type synonyms resolved)
      -- Local:
    ctxTypeVars :: [Id],                    -- ^ free type variables
    ctxIns :: Map Id Type,                  -- ^ input parameter types
    ctxLocals :: Map Id Type,               -- ^ local variable types
    ctxModifies :: [Id],                    -- ^ variables in the modifies clause of the enclosing procedure
    ctxLabels :: [Id],                      -- ^ all labels of the enclosing procedure body
    ctxEncLabels :: [Id],                   -- ^ labels of all enclosing statements
    ctxTwoState :: Bool,                    -- ^ is the context two-state? (procedure body or postcondition)
    ctxInLoop :: Bool,                      -- ^ is context inside a loop body?
    ctxPos :: SourcePos,                    -- ^ position in the source code
    -- Persistent context (not specific to any node, never gets restored):
    ctxFreshTVCount :: Integer              -- ^ number of fresh type variables already generated
  }

-- | Empty context
emptyContext = Context {
    ctxTypeConstructors = M.empty,
    ctxTypeSynonyms     = M.empty,
    ctxGlobals          = M.empty,
    ctxConstants        = M.empty,
    ctxFunctions        = M.empty,
    ctxProcedures       = M.empty,
    ctxTypeVars         = [],
    ctxIns              = M.empty,
    ctxLocals           = M.empty,
    ctxModifies         = [],
    ctxLabels           = [],
    ctxEncLabels        = [],
    ctxTwoState         = False,
    ctxInLoop           = False,
    ctxPos              = noPos,
    ctxFreshTVCount     = 0
  }

setGlobals g ctx    = ctx { ctxGlobals = g }
setConstants c ctx  = ctx { ctxConstants = c }
setTypeVars tv ctx  = ctx { ctxTypeVars = tv }
setIns i ctx        = ctx { ctxIns = i }
setLocals l ctx     = ctx { ctxLocals = l }
setModifies m ctx   = ctx { ctxModifies = m }
setLabels lbs ctx   = ctx { ctxLabels = lbs }
setTwoState b ctx   = ctx { ctxTwoState = b }
setInLoop b ctx     = ctx { ctxInLoop = b }
setPos p ctx        = ctx { ctxPos = p }

-- | Type constructors and synonyms
typeNames c = M.keys (ctxTypeConstructors c) ++ M.keys (ctxTypeSynonyms c)
-- | Global variables and constants
globalScope c = M.union (ctxGlobals c) (ctxConstants c)
-- | Input parameters and local variables
localScope c = M.union (ctxIns c) (ctxLocals c)
-- | All variables that can be assigned to (local variables and global variables)
mutableVars c = M.union (ctxGlobals c) (ctxLocals c)
-- | All variables that can have where clauses (everything except constants)
allVars c = M.union (localScope c) (ctxGlobals c)
-- | All variables and constants (local-scope preferred)
allNames c = M.union (localScope c) (globalScope c)
-- | Names of functions and procedures
funProcNames c = M.keys (ctxFunctions c) ++ M.keys (ctxProcedures c)
-- | Function signature by name
funSig name c = ctxFunctions c ! name
-- | Procedure signature by name
procSig name c = ctxProcedures c ! name

-- | Return a fresh type variable name and a modified context
genFreshTV c = (freshTVName $ ctxFreshTVCount c, c { ctxFreshTVCount = ctxFreshTVCount c + 1 })

-- | 'locally' @check@ : perform @check@ and then restores the scoped part of the context
locally :: Typing a -> Typing a
locally check = do
  c <- get
  res <- check `catchError` (\err -> restore c >> throwError err)
  restore c
  return res
  where
    restore c = do
      n <- gets ctxFreshTVCount
      put c { ctxFreshTVCount = n }

{- Errors -}

-- | Type error with a source position and a pretty-printed message
data TypeError = TypeError SourcePos Doc

instance ErrorList TypeError where
  listMsg s = [TypeError noPos (text s)]

-- | Pretty-printed type error
typeErrorDoc (TypeError pos msgDoc) = text "Type error in" <+> text (show pos) $+$ msgDoc

-- | Pretty-printed list of type errors
typeErrorsDoc errs = (vsep . punctuate linebreak . map typeErrorDoc) errs

-- | Throw a single type error
throwTypeError msgDoc = do
  pos <- gets ctxPos
  throwError [TypeError pos msgDoc]

-- | 'typeMismatch' @doc1 ts1 doc2 ts2 contextDoc@ : throw an error because types @ts1@ do not match @ts2@;
-- use @doc1@ and @doc2@ to describe @ts1@ and @ts2@ correspondingly; use @contextDoc@ to describe the context of mismatch
typeMismatch doc1 ts1 doc2 ts2 contextDoc = throwTypeError $
  text "Cannot match" <+> doc1 <+> dquotes (commaSep (map pretty ts1)) <+>
  text "against" <+>      doc2 <+> dquotes (commaSep (map pretty ts2)) <+>
  contextDoc

-- | Computation with typing context as state, which can result in either a list of type errors or a
type Typing a = ErrorT [TypeError] (State Context) a

{- Types -}

-- | Check that a type variable is fresh and add it to context
checkTypeVar :: Id -> Typing ()
checkTypeVar v = do
  typeNames <- gets typeNames
  typeVars <- gets ctxTypeVars
  if v `elem` typeNames
    then throwTypeError (text v <+> text "already used as a type constructor or synonym")
    else if  v `elem` typeVars
      then throwTypeError (text "Multiple decalartions of type variable" <+> text v)
      else modify $ setTypeVars (v : typeVars)

-- | Check that all type names exist and have correct number of arguments
checkType :: Type -> Typing ()
checkType (MapType tv domains range) = do
  mapAccum_ checkTypeVar tv
  mapAccum_ (locally . checkType) (domains ++ [range])
  if not (null missingTV)
    then throwTypeError (text "Type variable(s) must occur in the domains or range of the map type:" <+> commaSep (map text missingTV))
    else return ()
  where
    missingTV = filter (not . freeInComponents) tv
    freeInComponents v = any (v `isFreeIn`) (range : domains)
checkType (IdType name args) = do
  tv <- gets ctxTypeVars
  tc <- gets ctxTypeConstructors
  ts <- gets ctxTypeSynonyms
  cases tv tc ts
  where
    cases tv tc ts
      | name `elem` tv && null args = return ()
      | M.member name tc = let n = tc ! name in
        if n == length args
          then mapAccum_ (locally . checkType) args
          else throwTypeError (text "Wrong number of arguments" <+> int (length args) <+> text "given to the type constructor" <+> text name <+>  parens (text "expected" <+> int n))
      | M.member name ts = let formals = fst (ts ! name) in
        if length formals == length args
          then mapAccum_ (locally . checkType) args
          else throwTypeError (text "Wrong number of arguments " <+> int (length args) <+> text "given to the type synonym" <+> text name <+> parens (text "expected" <+> int (length formals)))
      | otherwise = throwTypeError (text "Not in scope: type constructor or synonym" <+> text name)
checkType _ = return ()

-- | 'resolve' @c t@ : type @t@ with all type synonyms resolved according to binding in @c@
resolve :: Context -> Type -> Type
resolve c (MapType tv domains range) = MapType tv (map (resolve c') domains) (resolve c' range)
  where c' = c { ctxTypeVars = ctxTypeVars c ++ tv }
resolve c (IdType name args)
  | name `elem` ctxTypeVars c = IdType name args
  | otherwise = case M.lookup name (ctxTypeSynonyms c) of
    Nothing -> IdType name (map (resolve c) args)
    Just (formals, t) -> resolve c (typeSubst (M.fromList (zip formals args)) t)
resolve _ t = t

-- | 'withFreshTV' @tv types@ : generate fresh names for @tv@ and replace their occurrences in @types@
withFreshTV :: [Id] -> [Type] -> Typing ([Id], [Type])
withFreshTV tv types = do
  tv' <- replicateM (length tv) (state $ genFreshTV)
  let binding = fromTVNames tv tv'
  return (tv', map (typeSubst binding) types)

-- | 'pInstance' @sig actuals lhss@ :
-- Instantiation of type variables in a procedure @sig@ given the actual arguments @actuals@ and call left-hand sides @lhss@
-- (type binding is returned in terms of original type variables of @sig@, so that types of locals can be calculated)
pInstance :: PSig -> [Expression] -> [Expression] -> Typing TypeBinding
pInstance sig actuals lhss = do
  actualTypes <- mapAccum (locally . checkExpression) noneType actuals
  lhssTypes <- mapAccum (locally . checkExpression) noneType lhss
  let name = psigName sig
  let tv = psigTypeVars sig
  let argTypes = psigArgTypes sig
  let retTypes = psigRetTypes sig
  (newTV, newParamTypes) <- withFreshTV tv (argTypes ++ retTypes)
  let (newArgTypes, newRetTypes) = splitAt (length argTypes) newParamTypes
  case unifier [] newArgTypes actualTypes of
    Nothing -> typeMismatch (text "in-parameter types") argTypes (text "actual argument types") actualTypes (text "in the call to" <+> text name)
    Just u1 -> case unifier [] (map (typeSubst u1) newRetTypes) lhssTypes of
      Nothing -> typeMismatch (text "out-parameter types") (map (typeSubst (renameTypeVars newTV tv u1)) retTypes) (text "call left-hand side types") lhssTypes (text "in the call to" <+> text name)
      Just u2 -> return $ renameTypeVars newTV tv (u1 `M.union` u2)

{- Expressions -}

-- | 'checkExpression' @c expr@ :
-- Check that @expr@ is a valid expression and return its type
-- (requires all types in the context be valid and type synonyms be resolved)
checkExpression :: Expression -> Typing Type
checkExpression (Pos pos e) = do
  modify $ setPos pos
  case e of
    Literal val -> return $ valueType val
    Logical t _ -> return t
    Var id -> checkVar id
    Application id args -> checkApplication id args
    MapSelection m args -> checkMapSelection m args
    MapUpdate m args val -> checkMapUpdate m args val
    Old e' -> checkOld e'
    IfExpr cond e1 e2 -> checkIfExpression cond e1 e2
    Coercion e t -> checkCoercion e t
    UnaryExpression op e1 -> checkUnaryExpression op e1
    BinaryExpression op e1 e2 -> checkBinaryExpression op e1 e2
    Quantified qop tv vars e -> checkQuantified qop tv vars e

checkVar :: Id -> Typing Type
checkVar id = do
  cnames <- gets allNames
  case M.lookup id cnames of
    Nothing -> throwTypeError (text "Not in scope: variable or constant" <+> text id)
    Just t -> return t

checkApplication :: Id -> [Expression] -> Typing Type
checkApplication name args = do
  functions <- gets ctxFunctions
  case M.lookup name functions of
    Nothing -> throwTypeError (text "Not in scope: function" <+> text name)
    Just (MapType tv argTypes retType) -> do
      actualTypes <- mapAccum (locally . checkExpression) noneType args
      (_, newRetType : newArgTypes) <- withFreshTV tv (retType : argTypes)
      case unifier [] newArgTypes actualTypes of
        Nothing -> typeMismatch (text "formal argument types") argTypes (text "actual argument types") actualTypes (text "in the call to" <+> text name)
        Just u -> return $ typeSubst u newRetType

checkMapSelection :: Expression -> [Expression] -> Typing Type
checkMapSelection m args = do
  mType <- locally $ checkExpression m
  selectTypes <- mapAccum (locally . checkExpression) noneType args
  case mType of
    MapType tv domainTypes rangeType -> do
      (_, newRangeType : newDomainTypes) <- withFreshTV tv (rangeType : domainTypes)
      case unifier [] newDomainTypes selectTypes of
        Nothing -> typeMismatch (text "map domain types") domainTypes (text "map selection types") selectTypes (text "for map" <+> pretty m)
        Just u -> return $ typeSubst u newRangeType
    t -> do
      freshRange <- nullaryType <$> state genFreshTV
      case unifier [] [t] [MapType [] selectTypes freshRange] of
        -- t is not a free variable:
        Nothing -> throwTypeError (text "Map selection applied to a non-map" <+> pretty m <+> text "of type" <+> dquotes (pretty t))
        -- t is a free variable:
        Just u -> return $ typeSubst u freshRange

checkMapUpdate :: Expression -> [Expression] -> Expression -> Typing Type
checkMapUpdate m args val = do
  mType <- locally $ checkExpression m
  selectTypes <- mapAccum (locally . checkExpression) noneType args
  updateType <- locally $ checkExpression val
  case mType of
    MapType tv domainTypes rangeType -> do
      (newTV, (newRangeType : newDomainTypes)) <- withFreshTV tv (rangeType : domainTypes)
      case unifier [] newDomainTypes selectTypes of
        Nothing -> typeMismatch (text "map domain types") domainTypes (text "map selection types") selectTypes (text "for map" <+> pretty m)
        Just u1 -> case unifier [] [typeSubst u1 newRangeType] [updateType] of
          Nothing -> typeMismatch (text "map range type") [typeSubst (renameTypeVars newTV tv u1) rangeType] (text "map update type") [updateType] (text "for map" <+> pretty m)
          Just u2 -> return $ typeSubst (u1 `M.union` u2) mType -- mType does not contain fresh names for tv, so only free type variables that came from outside will be substituted
    t -> do
      case unifier [] [t] [MapType [] selectTypes updateType] of
        -- t is not a free variable:
        Nothing -> throwTypeError (text "Map update applied to a non-map" <+> pretty m <+> text "of type" <+> dquotes (pretty t))
        -- t is a free variable:
        Just u -> return $ typeSubst u t

checkOld :: Expression -> Typing Type
checkOld e = do
  twoState <- gets ctxTwoState
  if twoState
    then do
      modify $ setLocals M.empty
      locally $ checkExpression e
    else throwTypeError (text "Old expression in a single state context")

checkIfExpression :: Expression -> Expression -> Expression -> Typing Type
checkIfExpression cond e1 e2 = do
  locally $ checkMatch (text "if-expression condition") BoolType cond
  t1 <- locally $ checkExpression e1
  t2 <- locally $ checkExpression e2
  case unifier [] [t1] [t2] of
    Nothing -> typeMismatch (text "type of then-part") [t1] (text "type of else-part") [t2] (text "in if-expression")
    Just u -> return $ typeSubst u t1

checkCoercion :: Expression -> Type -> Typing Type
checkCoercion e t = do
  locally $ checkType t
  t' <- (flip resolve) t <$> get
  locally $ checkMatch (text "coerced expression") t' e
  return t'

checkUnaryExpression :: UnOp -> Expression -> Typing Type
checkUnaryExpression op e
  | op == Neg = checkMatch (msg op) IntType e >> return IntType
  | op == Not = checkMatch (msg op) BoolType e >> return BoolType
  where
    msg op = text "operand to" <+> pretty op

checkBinaryExpression :: BinOp -> Expression -> Expression -> Typing Type
checkBinaryExpression op e1 e2
  | elem op [Plus, Minus, Times, Div, Mod] = matchOperands IntType IntType IntType
  | elem op [And, Or, Implies, Explies, Equiv] = matchOperands BoolType BoolType BoolType
  | elem op [Ls, Leq, Gt, Geq] = matchOperands IntType IntType BoolType
  | elem op [Eq, Neq] = do
    ctv <- gets ctxTypeVars;
    t1 <- locally $ checkExpression e1;
    t2 <- locally $ checkExpression e2;
    case unifier ctv [t1] [t2] of
      Nothing -> typeMismatch (text "type of left operand") [t1] (text "type of right operand") [t2] (text "to" <+> pretty op)
      Just _ -> return BoolType
  | op == Lc = do
    t1 <- locally $ checkExpression e1;
    t2 <- locally $ checkExpression e2;
    if isJust $ unifier [] [t1] [t2]
      then return BoolType
      else typeMismatch (text "type of left operand") [t1] (text "type of right operand") [t2] (text "to" <+> pretty op)
  where
    matchOperands t1 t2 ret = do
      locally $ checkMatch (msgLeft op) t1 e1
      locally $ checkMatch (msgRight op) t2 e2
      return ret
    msgLeft op = text "left operand to" <+> pretty op
    msgRight op = text "right operand to" <+> pretty op

checkQuantified :: QOp -> [Id] -> [IdType] -> Expression -> Typing Type
checkQuantified Lambda tv vars e = do
  mapAccum_ checkTypeVar tv
  mapAccum_ (checkIdType localScope ctxIns setIns) vars
  if not (null missingTV)
    then throwTypeError (text "Type variable(s) must occur among the types of lambda parameters:" <+> commaSep (map text missingTV))
    else do
      rangeType <- locally $ checkExpression e
      return $ MapType tv varTypes rangeType
  where
    varTypes = map snd vars
    missingTV = filter (not . freeInVars) tv
    freeInVars v = any (v `isFreeIn`) varTypes
checkQuantified qop tv vars e = do
  mapAccum_ checkTypeVar tv
  mapAccum_ (checkIdType localScope ctxIns setIns) vars
  checkMatch (text "scoped expression") BoolType e
  return BoolType

{- Statements -}

-- | 'checkStatement' @c st@ :
-- Check that @st@ is a valid statement
checkStatement :: Statement -> Typing ()
checkStatement (Pos pos s) = do
  modify $ setPos pos
  case s of
    Predicate _ (SpecClause _ _ e) -> checkMatch (text "predicate") BoolType e
    Havoc vars -> checkLefts (nub vars) (length (nub vars))
    Assign lhss rhss -> checkAssign lhss rhss
    Call lhss name args -> checkCall lhss name args
    CallForall name args -> checkCallForall name args
    If cond thenBlock elseBlock -> checkIf cond thenBlock elseBlock
    While cond invs b -> checkWhile cond invs b
    Goto ids -> checkGoto ids
    Break Nothing -> checkSimpleBreak
    Break (Just l) -> checkLabelBreak l
    _ -> return ()

checkAssign :: [(Id , [[Expression]])] -> [Expression] -> Typing ()
checkAssign lhss rhss = do
  locally $ checkLefts (map fst lhss) (length rhss)
  rTypes <- mapAccum (locally . checkExpression) noneType rhss
  cpos <- gets ctxPos
  let selectExpr (id, selects) = foldl mapSelectExpr (attachPos cpos (Var id)) selects
  zipWithAccum_ (\t e -> locally $ checkMatch (text "assignment left-hand side") t e) rTypes (map selectExpr lhss)

checkCall :: [Id] -> Id -> [Expression] -> Typing ()
checkCall lhss name args = do
  cprocs <- gets ctxProcedures
  case M.lookup name cprocs of
    Nothing -> throwTypeError (text "Not in scope: procedure" <+> text name)
    Just sig -> do
      cmods <- gets ctxModifies
      let illegalModifies = psigModifies sig \\ cmods
      if not (null illegalModifies)
        then throwTypeError (text "Call modifies a global variable that is not in the enclosing procedure's modifies clause:" <+> commaSep (map text illegalModifies))
        else do
          locally $ checkLefts lhss (length $ psigRetTypes sig)
          pos <- gets ctxPos
          let lhssExpr = map (attachPos pos . Var) lhss
          pInstance sig args lhssExpr >> return ()

checkCallForall :: Id -> [WildcardExpression] -> Typing ()
checkCallForall name args = do
  cprocs <- gets ctxProcedures
  case M.lookup name cprocs of
    Nothing -> throwTypeError (text "Not in scope: procedure" <+> text name)
    Just sig -> if not (null (psigModifies sig))
      then throwTypeError (text "Call forall to a procedure with a non-empty modifies clause")
      else pInstance sig { psigArgs = concrete (psigArgs sig) } concreteArgs [] >> return ()
  where
    concreteArgs = [e | (Expr e) <- args]
    concrete at = [at !! i | i <- [0..length args - 1], isConcrete (args !! i)]
    isConcrete Wildcard = False
    isConcrete (Expr _) = True

checkIf :: WildcardExpression -> Block -> (Maybe Block) -> Typing ()
checkIf cond thenBlock elseBlock = report $ do
  case cond of
    Wildcard -> return ()
    Expr e -> accum (locally $ checkMatch (text "branching condition") BoolType e) ()
  accum (locally $ checkBlock thenBlock) ()
  case elseBlock of
    Nothing -> return ()
    Just b -> accum (locally $ checkBlock b) ()

checkWhile :: WildcardExpression -> [SpecClause] -> Block -> Typing ()
checkWhile cond invs body = report $ do
  case cond of
    Wildcard -> return ()
    Expr e -> accum (locally $ checkMatch (text "loop condition") BoolType e) ()
  mapAccumA_ (locally . checkMatch (text "loop invariant") BoolType) (map specExpr invs)
  lift . modify $ setInLoop True
  accum (checkBlock body) ()

checkGoto :: [Id] -> Typing ()
checkGoto ids = do
  clbs <- gets ctxLabels
  let unknownLabels = ids \\ clbs
  if not (null unknownLabels)
    then throwTypeError (text "Not in scope: label(s)" <+> commaSep (map text unknownLabels))
    else return ()

checkSimpleBreak :: Typing ()
checkSimpleBreak = do
  inLoop <- gets ctxInLoop
  if not inLoop
    then throwTypeError (text "Break statement outside a loop")
    else return ()

checkLabelBreak :: Id -> Typing ()
checkLabelBreak l = do
  clbs <- gets ctxEncLabels
  if not (l `elem` clbs)
    then throwTypeError (text "Break label" <+> text l <+> text "does not label an enclosing statement")
    else return ()

{- Blocks -}

-- | 'collectLabels' @block@ :
-- Check that all labels in @block@ and nested blocks are unique and add them to the context
collectLabels :: Block -> Typing ()
collectLabels block = mapAccum_ checkLStatement block
  where
    checkLStatement (Pos pos (ls, st)) = do
      modify $ setPos pos
      mapM_ addLabel ls
      case node st of
        If _ thenBlock mElseBlock -> do
          collectLabels thenBlock
          case mElseBlock of
            Nothing -> return ()
            Just elseBlock -> collectLabels elseBlock
        While _ _ bodyBlock -> collectLabels bodyBlock
        _ -> return ()
    addLabel l = do
      clbs <- gets ctxLabels
      if l `elem` clbs
        then throwTypeError (text "Multiple occurrences of label" <+> text l <+> text "in a procedure body")
        else modify $ setLabels (l : clbs)

-- | Check every statement in a block
checkBlock :: Block -> Typing ()
checkBlock block = mapAccum_ (locally . checkLStatement) block
  where
    checkLStatement (Pos _ (ls, st)) = do
      modify $ \c -> c { ctxEncLabels = ctxEncLabels c ++ ls }
      checkStatement st

{- Declarations -}

-- | Collect type names from type declarations
collectTypes :: Decl -> Typing ()
collectTypes (Pos pos d) = do
  modify $ setPos pos
  case d of
    TypeDecl ts -> mapM_ checkTypeDecl ts
    otherwise -> return ()

-- | Check uniqueness of type constructors and synonyms, and them in the context
checkTypeDecl :: NewType -> Typing ()
checkTypeDecl (NewType name formals value) = do
  ctn <- gets typeNames
  if name `elem` ctn
    then throwTypeError (text "Multiple declarations of type constructor or synonym" <+> text name)
    else case value of
      Nothing -> modify $ \c -> c { ctxTypeConstructors = M.insert name (length formals) (ctxTypeConstructors c) }
      Just t -> modify $ \c -> c { ctxTypeSynonyms = M.insert name (formals, t) (ctxTypeSynonyms c) }

-- | Check that type arguments of type synonyms are fresh and values are valid types
checkTypeSynonyms :: Decl -> Typing ()
checkTypeSynonyms (Pos pos d) = do
  modify $ setPos pos
  case d of
    TypeDecl ts -> mapAccum_ (locally . checkNewType) ts
    otherwise -> return ()
  where
    checkNewType (NewType name formals (Just t)) = do
      mapAccum_ checkTypeVar formals
      checkType t
    checkNewType _ = return ()

-- | Check if type synonym declarations have cyclic dependences (program is passed for the purpose of error reporting)
checkCycles :: [Decl] -> Id -> Typing ()
checkCycles decls id = do
  typeSynonyms <- gets ctxTypeSynonyms
  checkCyclesWith typeSynonyms id (value typeSynonyms id)
  where
    checkCyclesWith typeSynonyms id t = case t of
      IdType name args -> do
        locally $ if M.member name typeSynonyms
          then if id == name
            then do
              modify $ setPos firstPos
              throwTypeError (text "Cycle in the definition of type synonym" <+> text id)
            else checkCyclesWith typeSynonyms id (value typeSynonyms name)
          else return ()
        mapAccum_ (locally . checkCyclesWith typeSynonyms id) args
      MapType _ domains range -> mapAccum_ (locally . checkCyclesWith typeSynonyms id) (range:domains)
      _ -> return ()
    value typeSynonyms name = snd (typeSynonyms ! name)
    firstPos = head [pos | Pos pos (TypeDecl ts) <- decls, id `elem` map tId ts]

-- | Check variable, constant, function and procedures and add them to context
checkSignatures :: Decl -> Typing ()
checkSignatures (Pos pos d) = do
  modify $ setPos pos
  case d of
    VarDecl vars -> mapAccum_ (checkIdType globalScope ctxGlobals setGlobals) (map noWhere vars)
    ConstantDecl _ ids t _ _ -> mapAccum_ (checkIdType globalScope ctxConstants setConstants) (zip ids (repeat t))
    FunctionDecl _ name tv args ret _ -> checkFunctionSignature name tv args ret
    ProcedureDecl name tv args rets specs _ -> checkProcSignature name tv args rets specs
    otherwise -> return ()

-- | 'checkIdType' @scope getter setter idType@ :
-- Check that declaration @idType@ is fresh in @scope@, and if so add it to @getter@ using @setter@
checkIdType :: (Context -> Map Id Type) -> (Context -> Map Id Type) -> (Map Id Type -> Context -> Context) -> IdType -> Typing ()
checkIdType scope getter setter (i, t) = do
  s <- gets scope
  if M.member i s
    then throwTypeError (text "Multiple declarations of variable or constant" <+> text i)
    else do
      locally $ checkType t
      modify $ \c -> M.insert i (resolve c t) (getter c) `setter` c

-- | Check uniqueness of function name, types of formals and add function to context
checkFunctionSignature :: Id -> [Id] -> [FArg] -> FArg -> Typing ()
checkFunctionSignature name tv args ret = do
  cnames <- gets funProcNames
  if name `elem` cnames
    then throwTypeError (text "Multiple declarations of function or procedure" <+> text name)
    else do
      locally checkParams
      let freeInParams v = any (v `isFreeIn`) (map snd params)
      let missingTV = filter (not . freeInParams) tv
      if not (null missingTV)
        then throwTypeError (text "Type variable(s) must occur in function arguments or return type:" <+> commaSep (map text missingTV))
        else do
          argTypes <- gets $ \c -> map (resolve c . snd) args
          retType <- gets $ \c -> (resolve c . snd) ret
          modify $ addFSig name (MapType tv argTypes retType)
  where
    params = args ++ [ret]
    checkParams = do
      mapAccum_ checkTypeVar tv
      mapAccum_ checkFArg params
    checkFArg (Just id, t) = checkIdType ctxIns ctxIns setIns (id, t)
    checkFArg (Nothing, t) = locally $ checkType t
    addFSig name sig c = c { ctxFunctions = M.insert name sig (ctxFunctions c) }

-- | Check uniqueness of procedure name, types of formals and add procedure to context
checkProcSignature :: Id -> [Id] -> [IdTypeWhere] -> [IdTypeWhere] -> [Contract] -> Typing ()
checkProcSignature name tv args rets specs = do
  cnames <- gets funProcNames
  if name `elem` cnames
    then throwTypeError (text "Multiple declarations of function or procedure" <+> text name)
    else do
      locally checkParams
      let freeInParams v = any (v `isFreeIn`) (map itwType params)
      let missingTV = filter (not . freeInParams) tv
      if not (null missingTV)
        then throwTypeError(text "Type variable(s) must occur in procedure in- our out-parameters:" <+> commaSep (map text missingTV))
        else do
          argTypes <- gets $ \c -> map (mapItwType (resolve c)) args
          retTypes <- gets $ \c -> map (mapItwType (resolve c)) rets
          modify $ addPSig name (PSig name tv argTypes retTypes specs)
  where
    params = args ++ rets
    checkParams = do
      mapAccum_ checkTypeVar tv
      mapAccum_ checkPArg params
    checkPArg arg = checkIdType ctxIns ctxIns setIns (noWhere arg)
    addPSig name sig c = c { ctxProcedures = M.insert name sig (ctxProcedures c) }

-- | Check axioms, function and procedure bodies
checkBodies :: Decl -> Typing ()
checkBodies (Pos pos d) = do
  modify $ setPos pos
  case d of
    VarDecl vars -> mapAccum_ checkWhere vars
    ConstantDecl _ ids t (Just edges) _ -> locally $ checkParentInfo ids t (map snd edges)
    AxiomDecl e -> locally $ checkAxiom e
    FunctionDecl _ name tv args _ (Just body) -> locally $ checkFunction name tv args body
    ProcedureDecl name tv args rets specs mb -> locally $ checkProcedure tv args rets specs mb
    ImplementationDecl name tv args rets bodies -> locally $ checkImplementation name tv args rets bodies
    otherwise -> return ()

-- | Check that where-part is a valid boolean expression
checkWhere :: IdTypeWhere -> Typing ()
checkWhere var = do
  locally $ do
    modify $ setTwoState False
    checkMatch (text "where clause") BoolType (itwWhere var)

-- | 'checkParentInfo' @ids t parents@ : Check that identifiers in @parents@ are distinct constants of type @t@ and do not occur among @ids@
checkParentInfo :: [Id] -> Type -> [Id] -> Typing ()
checkParentInfo ids t parents = if length parents /= length (nub parents)
  then throwTypeError (text "Parent list contains duplicates:" <+> commaSep (map text parents))
  else mapAccum_ (locally . checkParent) parents
  where
    checkParent p = do
      cconst <- gets ctxConstants
      case M.lookup p cconst of
        Nothing -> throwTypeError (text "Not in scope: constant" <+> text p)
        Just t' -> if isJust $ unifier [] [t] [t']
          then if p `elem` ids
            then throwTypeError (text "Constant" <+> text p <+> text "is decalred to be its own parent")
            else return ()
          else typeMismatch (text "type of parent" <+> text p) [t'] (text "constant type") [t] empty

-- | Check that axiom is a valid boolean expression
checkAxiom :: Expression -> Typing ()
checkAxiom e = do
  modify $ setGlobals M.empty
  checkMatch (text "axiom") BoolType e

-- | Check that function body is a valid expression of the same type as the function return type
checkFunction :: Id -> [Id] -> [FArg] -> Expression -> Typing ()
checkFunction name tv args body = do
  modify $ setTypeVars tv
  mapAccum_ addFArg args
  modify $ setGlobals M.empty
  (MapType _ _ retType) <- gets $ funSig name
  checkMatch (text "function body") retType body
  where
    addFArg (Just id, t) = checkIdType ctxIns ctxIns setIns (id, t)
    addFArg _ = return ()

-- | Check where-parts of procedure arguments and statements in its body
checkProcedure :: [Id] -> [IdTypeWhere] -> [IdTypeWhere] -> [Contract] -> (Maybe Body) -> Typing ()
checkProcedure tv args rets specs mb = do
  modify $ setTypeVars tv
  mapAccum_ (checkIdType localScope ctxIns setIns) (map noWhere args)
  locally $ mapAccum_ checkWhere args
  mapAccum_ (locally . checkMatch (text "precondition") BoolType . specExpr) (preconditions specs)
  mapAccum_ (checkIdType localScope ctxLocals setLocals) (map noWhere rets)
  locally $ mapAccum_ checkWhere rets
  modify $ setTwoState True
  mapAccum_ (locally . checkMatch (text "postcondition") BoolType . specExpr) (postconditions specs)
  cglobs <- gets ctxGlobals
  let invalidModifies = modifies specs \\ M.keys cglobs
  if not (null invalidModifies)
    then throwTypeError (text "Identifier in a modifies clause does not denote a global variable:" <+> commaSep (map text invalidModifies))
    else case mb of
      Nothing -> return ()
      Just body -> do
        modify $ setModifies (modifies specs)
        checkBody body

-- | Check procedure body
checkBody :: Body -> Typing ()
checkBody body = do
  mapAccum_ (checkIdType localScope ctxLocals setLocals) (map noWhere (concat (fst body)))
  locally $ mapAccum_ checkWhere (concat (fst body))
  collectLabels (snd body)
  checkBlock (snd body)

-- | Check that implementation corresponds to a known procedure and matches its signature, then check all bodies
checkImplementation :: Id -> [Id] -> [IdType] -> [IdType] -> [Body] -> Typing ()
checkImplementation name tv args rets bodies = do
  cprocs <- gets ctxProcedures
  case M.lookup name cprocs of
    Nothing -> throwTypeError (text "Not in scope: procedure" <+> text name)
    Just sig -> do
      argTypes <- gets $ \c -> map (resolve c . snd) args
      retTypes <- gets $ \c -> map (resolve c . snd) rets
      case unifier [] [psigType sig] [MapType tv argTypes (tupleType retTypes)] of
        Nothing -> throwTypeError (text "Could not match procedure signature" <+>
          dquotes (sigDoc (psigArgTypes sig) (psigRetTypes sig)) <+>
          text "against implementation signature" <+>
          dquotes (sigDoc argTypes retTypes) <+>
          text "in the implementation of" <+> text name)
        Just _ -> do
          modify $ setTypeVars tv
          mapAccum_ (checkIdType localScope ctxIns setIns) args
          mapAccum_ (checkIdType localScope ctxLocals setLocals) rets
          modify $ setTwoState True
          modify $ setModifies (psigModifies sig)
          mapAccum_ (locally . checkBody) bodies
  where
    sigDoc argTypes retTypes = parens (commaSep (map pretty argTypes)) <+>
      text "returns" <+>
      parens (commaSep (map pretty retTypes))

{- Program -}

-- | Check program in several passes
checkProgram :: Program -> Typing ()
checkProgram (Program decls) = do
  mapAccum_ collectTypes decls                          -- collect type names from type declarations
  locally $ mapAccum_ checkTypeSynonyms decls           -- check values of type synonyms
  typeSynonyms <- gets $ M.keys . ctxTypeSynonyms
  locally $ mapAccum_ (checkCycles decls) typeSynonyms  -- check that type synonyms do not form a cycle
  mapAccum_ checkSignatures decls                       -- check variable, constant, function and procedure signatures
  mapAccum_ checkBodies decls                           -- check axioms, function and procedure bodies, constant parent info

{- Misc -}

-- | 'checkMatch' @msg t e@
-- Check that @e@ is a valid expression and its type matches @t@;
-- in case of type error use @msg@ as a description for @e@
-- (requires type synonyms in t be resolved)
checkMatch :: Doc -> Type -> Expression -> Typing ()
checkMatch edoc t e = do
  t' <- locally $ checkExpression e
  if isJust $ unifier [] [t] [t']
    then return ()
    else typeMismatch (text "type of" <+> edoc) [t'] (text "expected type") [t] empty

-- 'checkLefts' @ids n@ :
-- Check that there are @n@ @ids@, all @ids@ are unique and denote mutable variables
checkLefts :: [Id] -> Int -> Typing ()
checkLefts vars n = if length vars /= n
  then throwTypeError (text "Expected" <+> int n <+> text "left-hand sides and got" <+> int (length vars))
  else if vars /= nub vars
    then throwTypeError (text "Variable occurs more than once among left-handes of a parallel assignment")
    else do
      mv <- gets $ M.keys . mutableVars
      let immutableLhss = vars \\ mv
      if not (null immutableLhss)
        then throwTypeError (text "Assignment to immutable variable(s):" <+> commaSep (map text immutableLhss))
        else do
          clocs <- gets $ M.keys . ctxLocals
          cmods <- gets ctxModifies
          let invalidGlobals = (vars \\ clocs) \\ cmods
          if not (null invalidGlobals)
            then throwTypeError (text "Assignment to a global variable that is not in the enclosing procedure's modifies clause:" <+> commaSep (map text invalidGlobals))
            else return ()

