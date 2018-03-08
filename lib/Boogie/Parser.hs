-- | Parsec-based parser for Boogie 2
module Boogie.Parser (
  program,
  type_,
  expression,
  statement,
  decl
) where

import Boogie.AST
import Boogie.Util
import Boogie.Position
import Boogie.Tokens
import Data.List
import Data.Map ((!), elems)
import Text.ParserCombinators.Parsec hiding (token, label)
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Expr
import Control.Monad
import Control.Applicative ((<$>), (<*>), (<*), (*>))

{- Interface -}

-- | Program parser
program :: Parser Program
program = do
  whiteSpace
  p <- many decl
  eof
  return $ Program p

{- Lexical analysis -}

opNames :: [String]
opNames = elems unOpTokens ++ (elems binOpTokens \\ keywords) ++ otherOps

opStart :: [Char]
opStart = nub (map head opNames)

opLetter :: [Char]
opLetter = nub (concatMap tail opNames)

boogieDef :: P.LanguageDef st
boogieDef = P.LanguageDef
  commentStart
  commentEnd
  commentLine
  False
  (letter <|> oneOf identifierChars)
  (alphaNum <|> oneOf identifierChars)
  (oneOf opStart)
  (oneOf opLetter)
  keywords
  opNames
  True

lexer :: P.TokenParser ()
lexer = P.makeTokenParser boogieDef

identifier = P.identifier lexer
reserved = P.reserved lexer
reservedOp = P.reservedOp lexer
charLiteral = P.charLiteral lexer
stringLiteral = P.stringLiteral lexer
natural = P.natural lexer
integer = P.integer lexer
symbol = P.symbol lexer
whiteSpace = P.whiteSpace lexer
angles = P.angles lexer
brackets = P.brackets lexer
parens = P.parens lexer
braces = P.braces lexer
semi = P.semi lexer
comma = P.comma lexer
commaSep = P.commaSep lexer
commaSep1 = P.commaSep1 lexer

{- Types -}

typeAtom :: Parser Type
typeAtom = choice [
  reserved "int" >> return IntType,
  reserved "bool" >> return BoolType,
  -- bit vector
  parens type_
  ]

typeArgs :: Parser [Id]
typeArgs = try (angles (commaSep1 identifier)) <|> return []

mapType :: Parser Type
mapType = do
  args <- typeArgs
  domains <- brackets (commaSep1 type_)
  range <- type_
  return $ MapType args domains range

typeCtorArgs :: Parser [Type]
typeCtorArgs = choice [
  do
    x <- typeAtom
    xs <- option [] typeCtorArgs
    return $ x : xs,
  do
    x <- identifier
    xs <- option [] typeCtorArgs
    return $ IdType x [] : xs,
  do
    x <- mapType
    return [x]
  ]

-- | Type parser
type_ :: Parser Type
type_ = choice [
  typeAtom,
  mapType,
  do
    id <- identifier
    args <- option [] typeCtorArgs
    return $ IdType id args
  ] <?> "type"

{- Expressions -}

qop :: Parser QOp
qop = (reserved "forall" >> return Forall) <|> (reserved "exists" >> return Exists) <|> (reserved "lambda" >> return Lambda)

atom :: Parser BareExpression
atom = choice [
  reserved "false" >> return ff,
  reserved "true" >> return tt,
  numeral <$> natural,
  varOrCall,
  old,
  ifThenElse,
  node <$> try (parens expression),
  parens quantified
  ]
  where
    varOrCall = do
      id <- identifier
      (parens (commaSep expression) >>= (return . Application id)) <|> (return $ Var id)
    old = do
      reserved "old"
      e <- parens expression
      return $ Old e
    ifThenElse = do
      reserved "if"
      cond <- expression
      reserved "then"
      thenExpr <- expression
      reserved "else"
      elseExpr <- expression
      return $ IfExpr cond thenExpr elseExpr
    quantified = do
      op <- qop
      args <- typeArgs
      vars <- case args of
        [] -> commaSep1 idsType
        _ -> commaSep idsType
      reservedOp "::"
      case op of
        Lambda -> return []
        _ -> many trigAttr
      e <- expression
      return $ Quantified op args (ungroup vars) e

arrayExpression :: Parser Expression
arrayExpression = do
  e <- attachPosBefore atom
  mapOps <- many (brackets (mapOp))
  return $ foldr (.) id (reverse mapOps) e
  where
    mapOp = do
      args <- commaSep1 expression
      option (inheritPos ((flip MapSelection) args)) (do
        reservedOp ":="
        e <- expression
        return $ inheritPos (flip ((flip MapUpdate) args) e))

coercionExpression :: Parser Expression
coercionExpression = do
  e <- arrayExpression
  coercedTos <- many coercedTo
  return $ foldr (.) id (reverse coercedTos) e
  where
    coercedTo = do
      reservedOp ":"
      t <- type_
      return $ inheritPos ((flip Coercion) t)

-- | Expression parser
expression :: Parser Expression
expression = buildExpressionParser table coercionExpression <?> "expression"

table = [[unOp Neg, unOp Not],
     [binOp Times AssocLeft, binOp Div AssocLeft, binOp Mod AssocLeft],
     [binOp Plus AssocLeft, binOp Minus AssocLeft],
     --[binOp Concat AssocLeft],
     [binOp Eq AssocNone, binOp Neq AssocNone, binOp Ls AssocNone, binOp Leq AssocNone, binOp Gt AssocNone, binOp Geq AssocNone, binOp Lc AssocNone],
     [binOp And AssocLeft], -- ToDo: && and || on the same level but do not interassociate
     [binOp Or AssocLeft],
     [binOp Implies AssocRight, binOp Explies AssocLeft], -- Mixing is prevented by different associativities
     [binOp Equiv AssocRight]]
  where
    binOp op assoc = Infix (reservedOp (binOpTokens ! op) >> return (\e1 e2 -> attachPos (position e1) (BinaryExpression op e1 e2))) assoc
    unOp op = Prefix (do
      pos <- getPosition
      reservedOp (unOpTokens ! op)
      return (\e -> attachPos pos (UnaryExpression op e)))

wildcardExpression :: Parser WildcardExpression
wildcardExpression = (expression >>= return . Expr) <|> (reservedOp "*" >> return Wildcard)

{- Statements -}

lhs :: Parser (Id, [[Expression]])
lhs = do
  id <- identifier
  selects <- many (brackets (commaSep1 expression))
  return (id, selects)

assign :: Parser BareStatement
assign = do
  lefts <- commaSep1 lhs
  reservedOp ":="
  rights <- commaSep1 expression
  semi
  return $ Assign lefts rights

call :: Parser BareStatement
call = do
  reserved "call"
  void (many attribute)
  lefts <- option [] (try lhss)
  id <- identifier
  args <- parens (commaSep expression)
  semi
  return $ Call lefts id args
  where
    lhss = do
      ids <- commaSep1 identifier
      reservedOp ":="
      return ids

callForall :: Parser BareStatement
callForall = do
  reserved "call"
  reserved "forall"
  id <- identifier
  args <- parens (commaSep wildcardExpression)
  semi
  return $ CallForall id args

ifStatement :: Parser BareStatement
ifStatement = do
  reserved "if"
  cond <- parens wildcardExpression
  thenBlock <- block
  elseBlock <- optionMaybe (reserved "else" >> (block <|> elseIf))
  return $ If cond thenBlock elseBlock
  where
    elseIf = do
      i <- attachPosBefore ifStatement
      return $ singletonBlock i

whileStatement :: Parser BareStatement
whileStatement = do
  reserved "while"
  cond <- parens wildcardExpression
  invs <- many loopInvariant
  body <- block
  return $ While cond invs body
  where
    loopInvariant = do
      free <- hasKeyword "free"
      reserved "invariant"
      e <- expression
      semi
      return (SpecClause LoopInvariant free e)

-- | Statement parser
statement :: Parser Statement
statement = attachPosBefore (choice [
  do { reserved "assert"; attrs <- many attribute; e <- expression; semi; return $ Predicate attrs (SpecClause Inline False e) },
  do { reserved "assume"; attrs <- many attribute; e <- expression; semi; return $ Predicate attrs (SpecClause Inline True e) },
  do { reserved "havoc"; ids <- commaSep1 identifier; semi; return $ Havoc ids },
  assign,
  try call,
  callForall,
  ifStatement,
  whileStatement,
  do { reserved "break"; id <- optionMaybe identifier; semi; return $ Break id },
  do { reserved "return"; semi; return Return },
  do { reserved "goto"; ids <- commaSep1 identifier; semi; return $ Goto ids }
  ] <?> "statement")

label :: Parser Id
label = do
  id <- identifier
  reservedOp ":"
  return id
  <?> "label"

lStatement :: Parser LStatement
lStatement = attachPosBefore $ do
  lbs <- many (try label)
  s <- statement
  return (lbs, s)

statementList :: Parser Block
statementList = do
  lstatements <- many (try lStatement)
  pos1 <- getPosition
  lempty <- many (try label)
  pos2 <- getPosition
  return $ if null lempty
    then lstatements
    else lstatements ++ [attachPos pos1 (lempty, attachPos pos2 Skip)]

block :: Parser Block
block = braces statementList

{- Declarations -}

newType :: Parser NewType
newType = do
  name <- identifier
  args <- many identifier
  value <- optionMaybe (reservedOp "=" >> type_ )
  return $ NewType name args value

typeDecl :: Parser BareDecl
typeDecl = do
  reserved "type"
  void (many attribute)
  ts <- commaSep newType
  semi
  return $ TypeDecl ts

parentEdge :: Parser ParentEdge
parentEdge = do
  unique <- hasKeyword "unique"
  id <- identifier
  return (unique, id)

constantDecl :: Parser BareDecl
constantDecl = do
  reserved "const"
  void (many attribute)
  unique <- hasKeyword "unique"
  ids <- idsType
  orderSpec <- optionMaybe (reserved "extends" >> commaSep parentEdge)
  complete <- hasKeyword "complete"
  semi
  return $ ConstantDecl unique (fst ids) (snd ids) orderSpec complete

functionDecl :: Parser BareDecl
functionDecl = do
  reserved "function"
  attrs <- many attribute
  name <- identifier
  tArgs <- typeArgs
  args <- parens (option [] (try namedArgs <|> unnamedArgs))
  ret <- returns <|> returnType
  body <- (semi >> return Nothing) <|> (Just <$> braces expression)
  return $ FunctionDecl attrs name tArgs args ret body
  where
    unnamedArgs = map (\t -> (Nothing, t))                  <$> commaSep1 type_
    namedArgs =   map (\(id, t) -> (Just id, t)) . ungroup  <$> commaSep1 idsType
    returns = do
      reserved "returns"
      parens fArg
    fArg = do
      name <- optionMaybe (try (identifier <* reservedOp ":"))
      t <- type_
      return (name, t)
    returnType = do
      reservedOp ":"
      t <- type_
      return (Nothing, t)

axiomDecl :: Parser BareDecl
axiomDecl = do
  reserved "axiom"
  void (many attribute)
  e <- expression
  semi
  return $ AxiomDecl e

varList :: Parser [IdTypeWhere]
varList = do
  reserved "var"
  void (many attribute)
  vars <- commaSep1 idsTypeWhere
  semi
  return $ ungroupWhere vars

varDecl :: Parser BareDecl
varDecl = VarDecl <$> varList

body :: Parser Body
body = braces (do
  locals <- many varList
  statements <- statementList
  return (locals, statements))

procDecl :: Parser BareDecl
procDecl = do
  reserved "procedure"
  void (many attribute)
  name <- identifier
  tArgs <- typeArgs
  args <- parens (commaSep idsTypeWhere)
  rets <- option [] (reserved "returns" >> parens (commaSep idsTypeWhere))
  noBody name tArgs args rets <|> withBody name tArgs args rets
  where
    noBody name tArgs args rets = do
      semi
      specs <- many spec
      return (ProcedureDecl name tArgs (ungroupWhere args) (ungroupWhere rets) specs Nothing)
    withBody name tArgs args rets = do
      specs <- many spec
      b <- body
      return (ProcedureDecl name tArgs (ungroupWhere args) (ungroupWhere rets) specs (Just b))

implDecl :: Parser BareDecl
implDecl = do
  reserved "implementation"
  void (many attribute)
  name <- identifier
  tArgs <- typeArgs
  args <- parens (commaSep idsType)
  rets <- option [] (reserved "returns" >> parens (commaSep idsType))
  bs <- many body
  return $ ImplementationDecl name tArgs (ungroup args) (ungroup rets) bs

-- | Top-level declaration parser
decl :: Parser Decl
decl = attachPosBefore (choice [
  typeDecl,
  constantDecl,
  functionDecl,
  axiomDecl,
  varDecl,
  procDecl,
  implDecl
  ] <?> "declaration")

{- Contracts -}

spec :: Parser Contract
spec = do
  free <- hasKeyword "free"
  choice [
    do
      reserved "requires"
      e <- expression
      semi
      return $ Requires free e,
    do
      reserved "modifies"
      ids <- commaSep identifier
      semi
      return $ Modifies free ids,
    do
      reserved "ensures"
      e <- expression
      semi
      return $ Ensures free e]

{- Misc -}

hasKeyword :: String -> Parser Bool
hasKeyword s = option False (reserved s >> return True)

idsType :: Parser ([Id], Type)
idsType = do
  ids <- commaSep1 identifier
  reservedOp ":"
  t <- type_
  return (ids, t)

ungroup :: [([Id], Type)] -> [(IdType)]
ungroup = concatMap (\x -> zip (fst x) (repeat (snd x)))

idsTypeWhere :: Parser ([Id], Type, Expression)
idsTypeWhere = do
  ids <- idsType
  pos <- getPosition
  e <- option (attachPos pos tt) (reserved "where" >> expression)
  return ((fst ids), (snd ids), e)

ungroupWhere :: [([Id], Type, Expression)] -> [IdTypeWhere]
ungroupWhere = concatMap ungroupWhereOne
  where ungroupWhereOne (ids, t, e) = zipWith3 IdTypeWhere ids (repeat t) (repeat e)

trigAttr :: Parser ()
trigAttr = (try trigger) <|> void attribute <?> "attribute or trigger"

trigger :: Parser ()
trigger = void (braces (commaSep1 expression)) <?> "trigger"

attribute :: Parser Attribute
attribute = (braces (do
  reservedOp ":"
  tag <- identifier
  vals <- commaSep ((EAttr <$> expression) <|> (SAttr <$> stringLiteral))
  return $ Attribute tag vals
  )) <?> "attribute"
