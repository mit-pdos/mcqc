module Codegen where
import Data.Aeson
import Prelude hiding (concat)
import Schema
import Data.ByteString.Lazy.Char8 (ByteString, unpack, pack, concat)

---- From casting with Module schema
-- Module {what = "module", name = "bt", need_magic = False, need_dummy = False,
--   used_modules = ["Datatypes"],
--   declarations = [
--     Declaration {what = "decl:ind", name = "tree", argnames = Just ["T"], typ = Nothing,
--       constructors = Just [Constructor {what = Nothing, name = "Leaf", argtypes = Just [], argnames = Nothing},
--                            Constructor {what = Nothing, name = "Node", argtypes = Just [Argtype {what = "type:glob", name = "tree", args = Just [Arg {what = "type:var", name = String "T"}]},Argtype {what = "type:var", name = "T", args = Nothing},Argtype {what = "type:glob", name = "tree", args = Just [Arg {what = "type:var", name = String "T"}]}], argnames = Nothing}]},
--
--     Declaration {what = "decl:term", name = "get_left", argnames = Nothing, typ = Just (Typ {what = "type:arrow", left = Argtype {what = "type:glob", name = "tree", args = Just [Arg {what = "type:varidx", name = Number 1.0}]}, right = Argtype {what = "type:glob", name = "tree", args = Just [Arg {what = "type:varidx", name = Number 1.0}]}}), constructors = Nothing},Declaration {what = "decl:term", name = "get_val", argnames = Nothing, typ = Just (Typ {what = "type:arrow", left = Argtype {what = "type:glob", name = "tree", args = Just [Arg {what = "type:varidx", name = Number 1.0}]}, right = Argtype {what = "type:glob", name = "Datatypes.option", args = Just [Arg {what = "type:varidx", name = Number 1.0}]}}), constructors = Nothing}]}
-- Pure codegen function to be used as Functor

-- Boogie 2 grammar
-- Decl ::= TypeDecl | ConstantDecl | FunctionDecl |AxiomDecl | VarDecl | ProcedureDecl | ImplementationDecl
-- TypeDecl ::= TypeConstructor | TypeSynonym
--

-- Coq to boogie Modules
codegen :: Module -> Either String ByteString
codegen Module { what = "module", used_modules = Nothing, declarations = decls } = mconcat <$> mapM codegenDecl decls
codegen Module { what = "module", used_modules = Just ml, declarations = decls } = mconcat <$> mapM codegenDecl decls -- TODO: Implement linking
codegen Module { what = s } = Left $ "Bad module 'what' key " ++ s
codegen m = Left $ unpack $ encode m

-- Coq to boogie declarations
codegenDecl :: Declaration -> Either String ByteString
codegenDecl Declaration { what = dt, name = dn, argnames = Just al, typ = Just tl, constructors = Just lc } =
    Right $ concat ["Declaration ", (pack dn), " of type ", pack dt, " argnames: ", " types", " constructors: ", "\n"]
codegenDecl Declaration { what = dt, name = dn, argnames = Just al, typ = Nothing, constructors = Just lc } =
    Right $ concat ["Declaration ", (pack dn), " of type ", pack dt, " argnames: ", " constructors", "\n"]
codegenDecl Declaration { what = dt, name = dn, argnames = Nothing, typ = Just t1, constructors = Just lc } =
    Right $ concat ["Declaration ", (pack dn), " of type ", pack dt, " types: ", " constructors", "\n"]
codegenDecl Declaration { what = dt, name = dn, argnames = Nothing, typ = Nothing, constructors = Just lc } =
    Right $ concat ["Declaration ", (pack dn), " of type ", pack dt, " constructors", "\n"]
codegenDecl Declaration { what = dt, name = dn, argnames = Just al, typ = Just tl, constructors = Nothing } =
    Right $ concat ["Declaration ", (pack dn), " of type ", pack dt, " argnames: ", " types: ", "\n"]
codegenDecl Declaration { what = dt, name = dn, argnames = Just al, typ = Nothing, constructors = Nothing } =
    Right $ concat ["Declaration ", (pack dn), " of type ", pack dt, " argnames: ", "\n"]
codegenDecl Declaration { what = dt, name = dn, argnames = Nothing, typ = Just t1, constructors = Nothing } =
    Right $ concat ["Declaration ", (pack dn), " of type ", pack dt, " types: ", "\n"]
codegenDecl Declaration { what = dt, name = dn, argnames = Nothing, typ = Nothing, constructors = Nothing } =
    Right $ concat ["Declaration ", (pack dn), " of type ", pack dt, "\n"]
codegenDecl d = Left $ unpack $ encode d

-- Parse JSON file into a Module
makeModule :: ByteString -> Either String Module
makeModule buffer = eitherDecode buffer :: Either String Module

