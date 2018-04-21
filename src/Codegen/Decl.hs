{-# LANGUAGE DuplicateRecordFields, OverloadedStrings, TemplateHaskell #-}
module Codegen.Decl (makeDecl) where
import Codegen.Ctor (makeIndCtor)
import Codegen.Schema
import Prelude hiding (concat)
import Data.ByteString.Lazy.Char8 (ByteString, concat, pack, unpack)

-- Coq to C++ inductive declarations (decl:ind)
-- TODO: Initialize constructors
makeIndDecl :: Declaration -> Either String ByteString
makeIndDecl Declaration { name = Just dn, argnames = Just al, constructors = Just lc } =
    mconcat <$> mapM (makeIndCtor dn) lc
makeIndDecl Declaration { name = Just dn, argnames = Just al, constructors = Nothing } =
    mconcat <$> mapM (makeIndCtor dn) []
makeIndDecl Declaration { name = Just dn, argnames = Nothing, constructors = Just lc } =
    mconcat <$> mapM (makeIndCtor dn) lc
makeIndDecl Declaration { name = Just dn, argnames = Nothing, constructors = Nothing } =
    Left $ "Found decl:ind " ++ dn ++ " with no arguments and no constructors"
makeIndDecl Declaration { name = Nothing, argnames = Nothing, constructors = Nothing } =
    Left $ "Found decl:ind with no name, arguments and no constructors"


-- Coq to C++ inductive type declarations (decl:type)
-- TODO: Initialize constructors
makeIndTypeDecl :: Declaration -> Either String ByteString
makeIndTypeDecl Declaration { name = Just dn, argnames = Just al, value = Just v } =
    Left $ "Found decl:type " ++ dn ++ " with arguments and value"
makeIndTypeDecl Declaration { name = Just dn, argnames = Just al, value = Nothing } =
    Left $ "Found decl:type " ++ dn ++ " with no arguments and no value"
makeIndTypeDecl Declaration { name = Just dn, argnames = Nothing, value = Just v } =
    Left $ "Found decl:type " ++ dn ++ " with no arguments and value"
makeIndTypeDecl Declaration { name = Just dn, argnames = Nothing, value = Nothing } =
    Left $ "Found decl:type " ++ dn ++ " with no arguments and no value"
makeIndTypeDecl Declaration { name = Nothing, argnames = Nothing, value = Nothing } =
    Left $ "Found decl:type with no name, arguments and no value"

-- Coq to C++ term declarations (decl:term)
-- TODO: Initialize constructors
makeTermDecl :: Declaration -> Either String ByteString
makeTermDecl Declaration { name = Just dn, typ = Just t, value = Just v } =
    Left $ "Found decl:term "
makeTermDecl Declaration { name = Just dn, typ = Just t, value = Nothing } =
    Left $ "Found decl:term "
makeTermDecl Declaration { name = Just dn, typ = Nothing, value = Just v } =
    Left $ "Found decl:term "
makeTermDecl Declaration { name = Just dn, typ = Nothing, value = Nothing } =
    Left $ "Found decl:term with no type and no value"
makeTermDecl Declaration { name = Nothing, typ = Nothing, value = Nothing } =
    Left $ "Found decl:term with no name, type and no value"

-- Coq to C++ fixpoint declarations (decl:fixgroup)
-- TODO: Initialize constructors
makeFixDecl :: Declaration -> Either String ByteString
makeFixDecl Declaration { fixlist = Just fl } =
    Left $ "Found decl:fixgroup "
makeFixDecl Declaration { fixlist = Nothing } =
    Left $ "Found decl:fixgroup with no fixlist"

makeDecl :: Declaration -> Either String ByteString
makeDecl d@Declaration { what = "decl:ind", name = Just dn } = makeIndDecl d
makeDecl d@Declaration { what = "decl:type", name = Just dn } = makeIndTypeDecl d
makeDecl d@Declaration { what = "decl:term", name = Just dn } = makeTermDecl d
makeDecl d@Declaration { what = "decl:fixgroup" } = makeFixDecl d
makeDecl   Declaration { what = wd } = Left $ "Found unhandled decl with 'what' type: " ++ wd

