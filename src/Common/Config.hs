{-# LANGUAGE OverloadedStrings #-}
module Common.Config where
import CIR.Expr
import Types.Context
import Data.Text (Text)
import qualified Data.Map as M

-- Libraries in include/*.hpp
libs :: [Text]
libs = [ "nat", "bool", "proc", "show", "option", "string", "variant", "pair" ]

-- Base types (pass-by-value)
base :: [Text]
base = [ "nat", "unit", "ascii", "bool", "proc", "string", "prod", "option" ]

-- Type context of native libraries
nativeContext :: Context CType
nativeContext = M.fromList [
    -- Ptr
    ("std::make_shared", [CTFree 1] --> (CTPtr $ CTFree 1)),
    -- Nat
    ("succ",      [CTBase "nat"] --> CTBase "nat"),
    ("pred",      [CTBase "nat"] --> CTBase "nat"),
    ("even",      [CTBase "nat"] --> CTBase "bool"),
    ("odd" ,      [CTBase "nat"] --> CTBase "bool"),
    ("add" ,      [CTBase "nat", CTBase "nat"] --> CTBase "nat"),
    ("sub" ,      [CTBase "nat", CTBase "nat"] --> CTBase "nat"),
    ("mul" ,      [CTBase "nat", CTBase "nat"] --> CTBase "nat"),
    ("div" ,      [CTBase "nat", CTBase "nat"] --> CTBase "nat"),
    ("mod" ,      [CTBase "nat", CTBase "nat"] --> CTBase "nat"),
    ("pow" ,      [CTBase "nat", CTBase "nat"] --> CTBase "nat"),
    ("eqb" ,      [CTBase "nat", CTBase "nat"] --> CTBase "bool"),
    ("leb" ,      [CTBase "nat", CTBase "nat"] --> CTBase "bool"),
    ("ltb" ,      [CTBase "nat", CTBase "nat"] --> CTBase "bool"),
    -- Bool
    ("negb",      [CTBase "bool"] --> CTBase "bool"),
    ("andb",      [CTBase "bool", CTBase "bool"] --> CTBase "bool"),
    ("orb",       [CTBase "bool", CTBase "bool"] --> CTBase "bool"),
    ("xorb",      [CTBase "bool", CTBase "bool"] --> CTBase "bool"),
    -- String
    ("append",    [CTBase "string", CTBase "string"] --> CTBase "string"),
    ("get",       [CTBase "nat", CTBase "string"] --> CTBase "char"),
    ("substring", [CTBase "nat", CTBase "nat", CTBase "string"] --> CTBase "string"),
    ("prefix",    [CTBase "string", CTBase "string"] --> CTBase "string"),
    ("prefix",    [CTBase "string", CTBase "string"] --> CTBase "bool"),
    ("prefix",    [CTBase "string", CTBase "string"] --> CTBase "bool"),
    ("prefix",    [CTBase "string", CTBase "string"] --> CTBase "bool"),
    ("length",    [CTBase "string"] --> CTBase "nat"),
    -- Proc
    ("open",      [CTBase "string"] --> CTExpr "proc" [CTBase "nat"]),
    ("socket",    [CTBase "nat"] --> CTExpr "proc" [CTBase "nat"]),
    ("read",      [CTBase "nat", CTBase "nat"] --> CTExpr "proc" [CTBase "string"]),
    ("write",     [CTBase "nat", CTBase "string"] --> CTExpr "proc" [CTBase "void"]),
    ("link",      [CTBase "string", CTBase "string"] --> CTExpr "proc" [CTBase "bool"]),
    ("unlink",    [CTBase "string"] --> CTExpr "proc" [CTBase "void"]),
    ("close",     [CTBase "nat"] --> CTExpr "proc" [CTBase "void"]),
    ("randnat",   [] --> CTExpr "proc" [CTBase "nat" ]),
    ("until",     [
                        [CTFree 1] --> CTBase "bool",
                        [CTExpr "option" [CTFree 1]] --> CTExpr "proc" [CTFree 1],
                        CTExpr "option" [CTFree 1]
                  ] --> CTExpr "proc" [CTFree 1]),
    ("spawn",     [CTFunc (CTBase "void") [CTFree 1], CTFree 1] --> CTExpr "proc" [CTBase "void"]),
    ("print",     [CTBase "string"] --> CTExpr "proc" [CTBase "void"]),
    ("ret",       [CTFree 1] --> CTExpr "proc" [CTFree 1]),
    ("bind",      [
                        CTExpr "proc" [CTFree 1],
                        [CTFree 1] --> CTExpr "proc" [CTFree 2]
                  ] --> CTExpr "proc" [CTFree 2]),
    -- Pairs
    ("fst",       [CTExpr "pair" [CTFree 1, CTFree 2]] --> CTFree 1),
    ("snd",       [CTExpr "pair" [CTFree 1, CTFree 2]] --> CTFree 2),
    -- Option
    ("some",      [CTFree 1] --> CTExpr "option" [CTFree 1]),
    ("none",      [] --> CTExpr "option" [CTFree 1]),
    -- Show is ad-hoc polymorphic but a parametric polymorphic type will do
    ("show",      [CTFree 1] --> CTBase "string")]

