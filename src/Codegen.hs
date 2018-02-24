module Codegen where
import Data.Aeson
import Prelude hiding (concat)
import Schema
import Utils
import Data.ByteString.Lazy.Char8 (ByteString, unpack, pack, concat)

---- From casting with Module schema
-- Module {what = "module", name = "bt", need_magic = False, need_dummy = False,
--   used_modules = ["Datatypes"],
--   declarations = [
--     Declaration {what = "decl:ind", name = "tree", argnames = Just ["T"], typ = Nothing,
--       constructors =
--          Just [Constructor {what = Nothing, name = "Leaf", argtypes = Just [], argnames = Nothing},
--                Constructor {what = Nothing, name = "Node", argtypes = Just [Argtype {what = "type:glob", name = "tree", args = Just [Arg {what = "type:var", name = String "T"}]},Argtype {what = "type:var", name = "T", args = Nothing},Argtype {what = "type:glob", name = "tree", args = Just [Arg {what = "type:var", name = String "T"}]}], argnames = Nothing}]},
--
--     Declaration {what = "decl:term", name = "get_left", argnames = Nothing, typ = Just (Typ {what = "type:arrow", left = Argtype {what = "type:glob", name = "tree", args = Just [Arg {what = "type:varidx", name = Number 1.0}]}, right = Argtype {what = "type:glob", name = "tree", args = Just [Arg {what = "type:varidx", name = Number 1.0}]}}), constructors = Nothing},Declaration {what = "decl:term", name = "get_val", argnames = Nothing, typ = Just (Typ {what = "type:arrow", left = Argtype {what = "type:glob", name = "tree", args = Just [Arg {what = "type:varidx", name = Number 1.0}]}, right = Argtype {what = "type:glob", name = "Datatypes.option", args = Just [Arg {what = "type:varidx", name = Number 1.0}]}}), constructors = Nothing}]}
-- Pure makeModule function to be used as Functor

-- Boogie 2 grammar
-- Decl ::= TypeDecl | ConstantDecl | FunctionDecl |AxiomDecl | VarDecl | ProcedureDecl | ImplementationDecl
-- TypeDecl ::= TypeConstructor | TypeSynonym
-- Coq to boogie Modules
makeModule :: Module -> Either String ByteString
makeModule Module { what = "module", used_modules = Nothing, declarations = decls } = mconcat <$> mapM makeDecl decls
makeModule Module { what = "module", used_modules = Just ml, declarations = decls } = mconcat <$> mapM makeDecl decls -- TODO: Implement linking
makeModule Module { what = s } = Left $ "Bad module 'what' key " ++ s

-- Coq to boogie declarations
-- TODO: Initialize constructors
makeDecl :: Declaration -> Either String ByteString
makeDecl Declaration { what = dt, name = dn, argnames = Just al, typ = Just tl, constructors = Just lc } =
    Right $ concat ["Declaration ", (pack dn), " of type ", pack $ dt, " argnames: ", " types", " constructors: ", "\n"]
makeDecl Declaration { what = dt, name = dn, argnames = Just al, typ = Nothing, constructors = Just lc } =
    Right $ concat ["Declaration ", (pack dn), " of type ", pack $ dt, " argnames: ", " constructors: ",  "\n"]
makeDecl Declaration { what = dt, name = dn, argnames = Nothing, typ = Just t1, constructors = Just lc } =
    Right $ concat ["Declaration ", (pack dn), " of type ", pack $ dt, " types: ", " constructors: ", "\n"]
makeDecl Declaration { what = dt, name = dn, argnames = Nothing, typ = Nothing, constructors = Just lc } =
    Right $ concat ["Declaration ", (pack dn), " of type ", pack $ dt, " constructors", "\n"]
makeDecl Declaration { what = dt, name = dn, argnames = Just al, typ = Just tl, constructors = Nothing } =
    Right $ concat ["Declaration ", (pack dn), " of type ", pack $ dt, " argnames: ", " types: ", "\n"]
makeDecl Declaration { what = dt, name = dn, argnames = Just al, typ = Nothing, constructors = Nothing } =
    Right $ concat ["Declaration ", (pack dn), " of type ", pack $ dt, " argnames: ", "\n"]
makeDecl Declaration { what = dt, name = dn, argnames = Nothing, typ = Just t1, constructors = Nothing } =
    Right $ concat ["Declaration ", (pack dn), " of type ", pack $ dt, " types: ", "\n"]
makeDecl Declaration { what = dt, name = dn, argnames = Nothing, typ = Nothing, constructors = Nothing } =
    Right $ concat ["Declaration ", (pack dn), " of type ", pack $ dt, "\n"]
makeDecl d = Left $ unpack $ encode d

-- Coq to boogie constructors
makeCtor :: String -> Constructor -> Either String ByteString
makeCtor pname Constructor { what = Just ct, name = cn, argtypes = Just at, argnames = Just an } =
    Right $ concat ["Constructor ", (pack cn), " of type ", (pack ct), " argtypes: ", " argnames: ", concat $ map pack an, "\n"]
makeCtor pname Constructor { what = Just ct, name = cn, argtypes = Just at, argnames = Nothing } =
    Right $ concat ["Constructor ", (pack cn), " of type ", (pack ct), " argtypes: ", "\n"]
makeCtor pname Constructor { what = Just ct, name = cn, argtypes = Nothing, argnames = Just an } =
    Right $ concat ["Constructor ", (pack cn), " of type ", (pack ct), " argnames: ", concat $ map pack an, "\n"]
makeCtor pname Constructor { what = Just ct, name = cn, argtypes = Nothing, argnames = Nothing } =
    Right $ concat ["Constructor ", (pack cn), " of type ", (pack ct), "\n"]
makeCtor pname Constructor { what = Nothing, name = cn, argtypes = Just at, argnames = Just an } =
    Right $ concat ["Constructor ", (pack cn), " argtypes: ", " argnames: ", concat $ map pack an, "\n"]
makeCtor pname Constructor { what = Nothing, name = cn, argtypes = Just at, argnames = Nothing } =
    Right $ concat ["Constructor ", (pack cn), " argtypes: ", "\n"]
makeCtor pname Constructor { what = Nothing, name = cn, argtypes = Nothing, argnames = Just an } =
    Right $ concat ["Constructor ", (pack cn), " argnames: ", concat $ map pack an, "\n"]
makeCtor pname Constructor { what = Nothing, name = cn, argtypes = Nothing, argnames = Nothing } =
    Right $ concat ["Constructor ", (pack cn), "\n"]

-- Parse JSON file into a Module
parse :: ByteString -> Either String Module
parse buffer = eitherDecode buffer :: Either String Module

