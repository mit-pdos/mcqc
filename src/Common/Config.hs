{-# LANGUAGE OverloadedStrings #-}
module Common.Config where
import CIR.Expr
import Types.Context
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M

-- Libraries in include/*.hpp
libs :: [Text]
libs = [ "nat", "bool", "io", "show", "option", "string", "variant", "pair" ]

-- Base types (pass-by-value)
base :: [Text]
base = [ "nat", "unit", "ascii", "bool", "io", "string", "prod", "option" ]

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
    -- IO
    ("open",      [CTBase "string"] --> CTExpr "IO" [CTBase "nat"]),
    ("socket",    [CTBase "nat"] --> CTExpr "IO" [CTBase "nat"]),
    ("read",      [CTBase "nat", CTBase "nat"] --> CTExpr "IO" [CTBase "string"]),
    ("write",     [CTBase "nat", CTBase "string"] --> CTExpr "IO" []),
    ("link",      [CTBase "string", CTBase "string"] --> CTExpr "IO" [CTBase "bool"]),
    ("unlink",    [CTBase "string"] --> CTExpr "IO" []),
    ("close",     [CTBase "nat"] --> CTExpr "IO" []),
    ("randnat",   [] --> CTExpr "IO" [CTBase "nat" ]),
    ("until",     [
                        [CTFree 1] --> CTBase "bool",
                        [CTExpr "option" [CTFree 1]] --> CTExpr "IO" [CTFree 1],
                        CTExpr "option" [CTFree 1]
                  ] --> CTExpr "IO" [CTFree 1]),
    ("spawn",     [[CTFree 1] --> (CTBase "void"), CTFree 1] --> CTExpr "IO" []),
    ("print",     [CTBase "string"] --> CTExpr "IO" []),
    ("ret",       [CTFree 1] --> CTExpr "IO" [CTFree 1]),
    ("bind",      [
                        CTExpr "IO" [CTFree 1],
                        [CTFree 1] --> CTExpr "IO" [CTFree 2]
                  ] --> CTExpr "IO" [CTFree 2]),
    -- Pairs
    ("fst",       [CTExpr "pair" [CTFree 1, CTFree 2]] --> CTFree 1),
    ("snd",       [CTExpr "pair" [CTFree 1, CTFree 2]] --> CTFree 2),
    -- Option
    ("some",      [CTFree 1] --> CTExpr "option" [CTFree 1]),
    ("none",      [] --> CTExpr "option" [CTFree 1]),
    -- Show is ad-hoc polymorphic but a parametric polymorphic type will do
    ("show",      [CTFree 1] --> CTBase "string")]

modpred :: String -> Bool
modpred ('M':t)
    | (T.toLower . T.pack $ t) `elem` libs = False
    | otherwise = True
modpred t
    | (T.toLower . T.pack $ t) `elem` base = False
    | otherwise = True

filterMod :: [Text] -> [Text]
filterMod = map T.pack . filter modpred . map T.unpack

