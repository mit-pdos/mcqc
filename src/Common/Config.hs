{-# LANGUAGE OverloadedStrings #-}
module Common.Config where
import CIR.Expr
import Types.Context
import Data.Text (Text)
import qualified Data.Map as M

-- Libraries in include/*.hpp
libs :: [Text]
libs = [ "nat", "bool", "proc", "show", "string", "variant", "pair" ]

-- Base types (pass-by-value)
base :: [Text]
base = [ "fd", "nat", "unit", "ascii", "bool", "proc", "string", "prod", "option" ]

-- Type context of native libraries
nativeContext :: Context CType
nativeContext = M.fromList [
    -- Ptr
    ("std::make_shared", CTFunc (CTPtr $ CTFree 1) [CTFree 1]),
    -- Nat
    ("succ",      CTFunc (CTBase "nat") [CTBase "nat"]),
    ("pred",      CTFunc (CTBase "nat") [CTBase "nat"]),
    ("even",      CTFunc (CTBase "bool") [CTBase "nat"]),
    ("odd" ,      CTFunc (CTBase "bool") [CTBase "nat"]),
    ("add" ,      CTFunc (CTBase "nat") [CTBase "nat", CTBase "nat"]),
    ("sub" ,      CTFunc (CTBase "nat") [CTBase "nat", CTBase "nat"]),
    ("mul" ,      CTFunc (CTBase "nat") [CTBase "nat", CTBase "nat"]),
    ("div" ,      CTFunc (CTBase "nat") [CTBase "nat", CTBase "nat"]),
    ("mod" ,      CTFunc (CTBase "nat") [CTBase "nat", CTBase "nat"]),
    ("pow" ,      CTFunc (CTBase "nat") [CTBase "nat", CTBase "nat"]),
    ("eqb" ,      CTFunc (CTBase "bool") [CTBase "nat", CTBase "nat"]),
    ("leb" ,      CTFunc (CTBase "bool") [CTBase "nat", CTBase "nat"]),
    ("ltb" ,      CTFunc (CTBase "bool") [CTBase "nat", CTBase "nat"]),
    -- Bool
    ("negb",      CTFunc (CTBase "bool") [CTBase "bool"]),
    ("andb",      CTFunc (CTBase "bool") [CTBase "bool", CTBase "bool"]),
    ("orb",      CTFunc (CTBase "bool") [CTBase "bool", CTBase "bool"]),
    ("xorb",      CTFunc (CTBase "bool") [CTBase "bool", CTBase "bool"]),
    -- String
    ("append",    CTFunc (CTBase "string") [CTBase "string", CTBase "string"]),
    ("get",       CTFunc (CTBase "char") [CTBase "nat", CTBase "string"]),
    ("substring", CTFunc (CTBase "string") [CTBase "nat", CTBase "nat", CTBase "string"]),
    ("prefix",    CTFunc (CTBase "bool") [CTBase "string", CTBase "string"]),
    ("prefix",    CTFunc (CTBase "bool") [CTBase "string", CTBase "string"]),
    ("prefix",    CTFunc (CTBase "bool") [CTBase "string", CTBase "string"]),
    ("prefix",    CTFunc (CTBase "bool") [CTBase "string", CTBase "string"]),
    ("size",      CTFunc (CTBase "nat") [CTBase "string"]),
    -- Proc
    ("open",      CTFunc (CTExpr "proc" [CTBase "nat"]) [CTBase "string"]),
    ("socket",    CTFunc (CTExpr "proc" [CTBase "nat"]) [CTBase "nat"]),
    ("read",      CTFunc (CTExpr "proc" [CTBase "string"]) [CTBase "nat", CTBase "nat"]),
    ("write",     CTFunc (CTExpr "proc" [CTBase "void"])   [CTBase "nat", CTBase "string"]),
    ("link",      CTFunc (CTExpr "proc" [CTBase "bool"])   [CTBase "string", CTBase "string"]),
    ("unlink",    CTFunc (CTExpr "proc" [CTBase "void"])   [CTBase "string"]),
    ("close",     CTFunc (CTExpr "proc" [CTBase "void"])   [CTBase "nat"]),
    ("randnat",   CTFunc (CTExpr "proc" [CTBase "nat" ])   []),
    ("until",     CTFunc (CTExpr "proc" [CTFree 1]) [
        CTFunc (CTBase "bool") [CTFree 1],
        CTFunc (CTExpr "proc" [CTFree 1]) [CTExpr "option" [CTFree 1]],
        CTExpr "option" [CTFree 1]
    ]),
    ("spawn",     CTFunc (CTExpr "proc" [CTBase "void"]) [CTFunc (CTBase "void") [CTFree 1], CTFree 1]),
    ("print",     CTFunc (CTExpr "proc" [CTBase "void"]) [CTBase "string"]),
    ("ret",       CTFunc (CTExpr "proc" [CTFree 1]) [CTFree 1]),
    ("bind",      CTFunc (CTExpr "proc" [CTFree 2]) [
        CTExpr "proc" [CTFree 1],
        CTFunc (CTExpr "proc" [CTFree 2]) [CTFree 1]
    ]),
    -- Pairs
    ("fst",       CTFunc (CTFree 1) [CTExpr "pair" [CTFree 1, CTFree 2]]),
    ("snd",       CTFunc (CTFree 2) [CTExpr "pair" [CTFree 1, CTFree 2]]),
    -- Option
    ("some",      CTFunc (CTExpr "option" [CTFree 1]) [CTFree 1]),
    ("none",      CTFunc (CTExpr "option" [CTFree 1]) []),
    -- Show
    -- Show is ad-hoc polymorphic but a parametric polymorphic type will do
    ("show",      CTFunc (CTBase "string") [CTFree 1])]
