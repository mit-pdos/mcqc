{-# LANGUAGE StandaloneDeriving, FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- | Abstract syntax tree for Boogie 2
module Boogie.AST where

import Boogie.Position

import Data.Data
import Data.Map (Map)
import qualified Data.Map as M
import Data.List

{- Basic -}

-- | Program: a list of top-level declarations
newtype Program = Program [Decl]
  deriving Eq

{- Types -}

-- | Types parametrized by the representation of free type variables
data GenType fv = 
  BoolType |                                -- ^ bool 
  IntType |                                 -- ^ int
  MapType [fv] [GenType fv] (GenType fv) |  -- 'MapType' @type_vars domains range@ : arrow type (used for maps, function and procedure signatures)
  IdType Id [GenType fv]                    -- 'IdType' @name args@: type denoted by an identifier (either type constructor, possibly with arguments, or a type variable)
  deriving (Data, Typeable)
  
-- | Regular types with free variables represented as identifiers 
type Type = GenType Id
  
-- | Type representation with nameless dummies
-- (using DeBrujn indexes).
-- This representation is invariant with respect to alpha-conversion and is used to derive instances of Eq and Ord.
deBrujn :: GenType Id -> GenType ()
deBrujn t = deBrujn' [] t
  where
    deBrujn' fv (MapType fv' domains range) = let newTv = fv ++ fv'
      in MapType [] (map (deBrujn' newTv) domains) (deBrujn' newTv range)
    deBrujn' fv (IdType name []) = case elemIndex name fv of
      Nothing -> IdType name []
      Just i -> dbIndex (length fv - i - 1)
    deBrujn' fv (IdType name args) = IdType name (map (deBrujn' fv) args)
    deBrujn' _ BoolType = BoolType
    deBrujn' _ IntType = IntType
    dbIndex i = IdType ("DB " ++ show i) []
    
deriving instance Eq (GenType ())
deriving instance Ord (GenType ())

instance Eq Type where
  t1 == t2 = deBrujn t1 == deBrujn t2  
  
instance Ord Type where
  compare t1 t2 = compare (deBrujn t1) (deBrujn t2)    
    
{- Expressions -}

-- | Unary operators
data UnOp = Neg | Not
  deriving (Eq, Ord, Data, Typeable)

-- | Binary operators  
data BinOp = Plus | Minus | Times | Div | Mod | And | Or | Implies | Explies | Equiv | Eq | Neq | Lc | Ls | Leq | Gt | Geq
  deriving (Eq, Ord, Data, Typeable)

-- | Quantifiers
data QOp = Forall | Exists | Lambda
  deriving (Eq, Ord, Data, Typeable)
  
-- | Expression with a source position attached  
type Expression = Pos BareExpression
  
-- | Expression
data BareExpression = 
  Literal Value |
  Var Id |                                        -- ^ 'Var' @name@
  Logical Type Ref |                              -- ^ Logical variable
  Application Id [Expression] |                   -- ^ 'Application' @f args@
  MapSelection Expression [Expression] |          -- ^ 'MapSelection' @map indexes@
  MapUpdate Expression [Expression] Expression |  -- ^ 'MapUpdate' @map indexes rhs@
  Old Expression |
  IfExpr Expression Expression Expression |       -- ^ 'IfExpr' @cond eThen eElse@
  Coercion Expression Type |
  UnaryExpression UnOp Expression |
  BinaryExpression BinOp Expression Expression |
  Quantified QOp [Id] [IdType] Expression         -- ^ 'Quantified' @qop type_vars bound_vars expr@
  deriving (Eq, Ord, Data, Typeable)  -- syntactic equality
  
-- | 'mapSelectExpr' @m args@ : map selection expression with position of @m@ attached
mapSelectExpr m args = attachPos (position m) (MapSelection m args)  

ff = Literal (BoolValue False)
tt = Literal (BoolValue True)
numeral n = Literal (IntValue n)

isLiteral (Pos _ (Literal _)) = True
isLiteral _ = False
fromLiteral (Pos _ (Literal v)) = v
  
-- | Wildcard or expression  
data WildcardExpression = Wildcard | Expr Expression
  deriving Eq
  
-- | Expressions without free variables  
type Thunk = Expression  
  
{- Statements -}

-- | Statement with a source position attached  
type Statement = Pos BareStatement

-- | Statement
data BareStatement = Predicate [Attribute] SpecClause |   -- ^ Predicate statement (assume or assert)
  Havoc [Id] |                                            -- ^ 'Havoc' @var_names@
  Assign [(Id , [[Expression]])] [Expression] |           -- ^ 'Assign' @var_map_selects rhss@
  Call [Id] Id [Expression] |                             -- ^ 'Call' @lhss proc_name args@
  CallForall Id [WildcardExpression] |                    -- ^ 'CallForall' @proc_name args@
  If WildcardExpression Block (Maybe Block) |             -- ^ 'If' @wild_or_expr then_block else_block@
  While WildcardExpression [SpecClause] Block |           -- ^ 'While' @wild_or_expr free_loop_inv loop_body@
  Break (Maybe Id) |                                      -- ^ 'Break' @label@
  Return |
  Goto [Id] |                                             -- ^ 'Goto' @labels@
  Skip                                                    -- ^ only used at the end of a block
  deriving Eq -- syntactic equality

-- | Statement labeled by multiple labels with a source position attached  
type LStatement = Pos BareLStatement

-- | Statement labeled by multiple labels
type BareLStatement = ([Id], Statement)

-- | Statement block
type Block = [LStatement] 

-- | Block consisting of a single non-labeled statement
singletonBlock s = [attachPos (position s) ([], s)]

-- | Procedure body: consists of local variable declarations and a statement block
type Body = ([[IdTypeWhere]], Block)

-- | Basic block is a list of statements labeled by a single label;
-- the list contains no jump, if or while statements,
-- except for the last statement, which can be a goto or return
type BasicBlock = (Id, [Statement])

-- | Procedure body transformed to basic blocks:
-- consists of local variable declarations and a set of basic blocks
-- (represented as a map from their labels to statement lists)
type BasicBody = ([IdTypeWhere], Map Id [Statement])

{- Specs -}

-- | Types of specification clauses
data SpecType = Inline | Precondition | Postcondition | LoopInvariant | Where | Axiom
  deriving Eq

-- | Specification clause
data SpecClause = SpecClause {
  specType :: SpecType,   -- ^ Source of the clause
  specFree :: Bool,       -- ^ Is it free (assumption) or checked (assertions)?
  specExpr :: Expression  -- ^ Boolean expression
  } deriving Eq

-- | Procedure contract clause 
data Contract = Requires Bool Expression |  -- ^ 'Requires' @e free@
  Modifies Bool [Id] |                      -- ^ 'Modifies' @var_names free@
  Ensures Bool Expression                   -- ^ 'Ensures' @e free@
  deriving Eq

{- Declarations -}

-- | Top-level declaration with a source position attached  
type Decl = Pos BareDecl

-- | Top-level declaration
data BareDecl = 
  TypeDecl [NewType] |
  ConstantDecl Bool [Id] Type ParentInfo Bool |                                -- ^ 'ConstantDecl' @unique names type orderSpec complete@
  FunctionDecl [Attribute] Id [Id] [FArg] FArg (Maybe Expression) |            -- ^ 'FunctionDecl' @name type_args formals ret body@
  AxiomDecl Expression |
  VarDecl [IdTypeWhere] |
  ProcedureDecl Id [Id] [IdTypeWhere] [IdTypeWhere] [Contract] (Maybe Body) |  -- ^ 'ProcedureDecl' @name type_args formals rets contract body@
  ImplementationDecl Id [Id] [IdType] [IdType] [Body]                          -- ^ 'ImplementationDecl' @name type_args formals rets body@
  deriving Eq
  
{- Values -}

-- | Reference (gives identity to things)
type Ref = Int

-- | Representation of a map value
type MapRepr = Map [Value] Value
  
-- | Representation of an empty map  
emptyMap = M.empty
  
-- | Run-time value
data Value = IntValue Integer |  -- ^ Integer value
  BoolValue Bool |               -- ^ Boolean value
  CustomValue Type Ref |         -- ^ Value of a user-defined type
  Reference Type Ref             -- ^ Map reference
  deriving (Eq, Ord, Data, Typeable)
  
-- | Type of a value
valueType :: Value -> Type
valueType (IntValue _) = IntType
valueType (BoolValue _) = BoolType
valueType (CustomValue t _) = t
valueType (Reference t _) = t
  
-- | 'valueFromInteger' @t n@: value of type @t@ with an integer code @n@
valueFromInteger :: Type -> Integer -> Value  
valueFromInteger IntType n        = IntValue n
valueFromInteger t@(IdType _ _) n = CustomValue t (fromInteger n)
valueFromInteger _ _              = error "cannot create a boolean or map value from integer" 
  
unValueInt (IntValue n) = n  
unValueBool (BoolValue b) = b

{- Attributes and triggers -}

-- | Attribute value
data AttrValue = EAttr Expression | SAttr String
  deriving Eq

-- | Attribute
data Attribute = Attribute {
  aTag :: Id,
  aValues :: [AttrValue]
  } deriving Eq
    
{- Misc -}

-- | Identifier
type Id = String

-- | Definition of a type
data NewType = NewType {
  tId :: Id,
  tArgs :: [Id],
  tValue :: Maybe Type
  } deriving Eq

-- | Name declaration (identifier, type)
type IdType = (Id, Type)

-- | Name declaration with a where clause
data IdTypeWhere = IdTypeWhere { 
  itwId :: Id, 
  itwType :: Type, 
  itwWhere :: Expression 
  } deriving Eq
  
-- | Strip the where clause  
noWhere itw = (itwId itw, itwType itw)
-- | Strip the type
noType itw = (itwId itw, itwWhere itw)  
  
-- | Formal argument of a function  
type FArg = (Maybe Id, Type)

-- | Argument name used for unnamed function arguments
dummyFArg = "arg"

-- | Parent edge of a constant declaration (uniqueness, parent name)
type ParentEdge = (Bool, Id)

-- | Parent information in a constant declaration
-- (Nothing means no information, while empty list means no parents)
type ParentInfo = Maybe [ParentEdge]

