-- | Tokens used in Boogie 2
module Boogie.Tokens where

import Boogie.AST
import Boogie.Pretty
import Data.Map (Map, (!), fromList)

-- | Keywords
keywords :: [String]
keywords = ["assert", "assume", "axiom", "bool", "break", "call", "complete", "const",
    "else", "div", "ensures", "exists", "extends", "false", "forall", "free", "function",
    "goto", "havoc", "if", "implementation", "int", "invariant", "lambda", "mod", "modifies",
    "old", "procedure", "requires", "return", "returns", "then", "true", "type", "unique",
    "var", "where", "while"
    ]

-- | Names of unary operators
unOpTokens :: Map UnOp String
unOpTokens = fromList [(Neg, "-")
                      ,(Not, "!")]

-- | Pretty-printed unary operator
instance Pretty UnOp where
  pretty op = text (unOpTokens ! op)

-- | Names of binary operators
binOpTokens :: Map BinOp String
binOpTokens = fromList [(Plus,    "+")
                       ,(Minus,   "-")
                       ,(Times,   "*")
                       ,(Div,     "div")
                       ,(Mod,     "mod")
                       ,(And,     "&&")
                       ,(Or,      "||")
                       ,(Implies, "==>")
                       ,(Explies, "<==")
                       ,(Equiv,   "<==>")
                       ,(Eq,      "==")
                       ,(Neq,     "!=")
                       ,(Lc,      "<:")
                       ,(Ls,      "<")
                       ,(Leq,     "<=")
                       ,(Gt,      ">")
                       ,(Geq,     ">=")]

-- | Pretty-printed binary operator
instance Pretty BinOp where
  pretty op = text (binOpTokens ! op)

-- | Names of quantifiers
qOpTokens :: Map QOp String
qOpTokens = fromList [(Forall, "forall")
                     ,(Exists, "exists")
                     ,(Lambda, "lambda")]

-- | Pretty-printed quantifier
instance Pretty QOp where
  pretty op = text (qOpTokens ! op)

-- | Other operators
otherOps :: [String]
otherOps = [":", ";", "::", ":=", "="]

-- | Characters allowed in identifiers (in addition to letters and digits)
identifierChars = "_.$#\'`~^\\?"
-- | Start of a multi-line comment
commentStart = "/*"
-- | End of a multi-line comment
commentEnd = "*/"
-- | Start of a single-line comment
commentLine = "//"

-- | A character that is not allowed in identifiers (used for generating unique names)
nonIdChar = '*'
