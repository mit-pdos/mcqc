{-# LANGUAGE DuplicateRecordFields, OverloadedStrings, TemplateHaskell #-}
module Codegen where
import Data.Aeson
import Prelude hiding (concat)
import Schema
import Utils
import Data.ByteString.Lazy.Char8 (ByteString, unpack, pack, concat)

-- Module high level function
makeModule :: Module -> Either String ByteString
makeModule Module { what = "module", used_modules = Nothing, declarations = decls } = mconcat <$> mapM makeDecl decls
makeModule Module { what = "module", used_modules = Just ml, declarations = decls } = mconcat <$> mapM makeDecl decls -- TODO: Implement linking
makeModule Module { what = s } = Left $ "Bad module 'what' key " ++ s

-- Coq to C++ declarations
-- TODO: Initialize constructors
makeDecl :: Declaration -> Either String ByteString
makeDecl Declaration { what = dt, name = Just dn, argnames = Just al, typ = Just tl, constructors = Just lc } =
    mconcat <$> mapM (makeCtor dn) lc
makeDecl Declaration { what = dt, name = Just dn, argnames = Just al, typ = Nothing, constructors = Just lc } =
    mconcat <$> mapM (makeCtor dn) lc
makeDecl Declaration { what = dt, name = Just dn, argnames = Nothing, typ = Just t1, constructors = Just lc } =
    mconcat <$> mapM (makeCtor dn) lc
makeDecl Declaration { what = dt, name = Just dn, argnames = Nothing, typ = Nothing, constructors = Just lc } =
    mconcat <$> mapM (makeCtor dn) lc
makeDecl Declaration { what = dt, name = Just dn, argnames = Just al, typ = Just tl, constructors = Nothing } =
    Right $ concat ["Declaration ", (pack dn), " of type ", pack $ dt, " argnames: ", " types: ", "\n"]
makeDecl Declaration { what = dt, name = Just dn, argnames = Just al, typ = Nothing, constructors = Nothing } =
    Right $ concat ["Declaration ", (pack dn), " of type ", pack $ dt, " argnames: ", "\n"]
makeDecl Declaration { what = dt, name = Just dn, argnames = Nothing, typ = Just t1, constructors = Nothing } =
    Right $ concat ["Declaration ", (pack dn), " of type ", pack $ dt, " types: ", "\n"]
makeDecl Declaration { what = dt, name = odn, argnames = Just al, typ = Just tl, constructors = Nothing } =
    Right $ concat ["Declaration of type ", pack $ dt, " argnames: ", " types: ", "\n"]
makeDecl Declaration { what = dt, name = odn, argnames = Just al, typ = Nothing, constructors = Nothing } =
    Right $ concat ["Declaration of type ", pack $ dt, " argnames: ", "\n"]
makeDecl Declaration { what = dt, name = odn, argnames = Nothing, typ = Just t1, constructors = Nothing } =
    Right $ concat ["Declaration of type ", pack $ dt, " types: ", "\n"]
makeDecl Declaration { what = dt, name = odn, argnames = Nothing, typ = Nothing, constructors = Nothing } =
    Right $ concat ["Declaration of type ", pack $ dt, "\n"]

-- Coq to C++ constructors
makeCtor :: String -> Constructor -> Either String ByteString
makeCtor pname Constructor { what = Just ct, name = cn, argtypes = Just at, argnames = Just an } =
    Right $ concat [" +++ ", (pack cn), " of type ", (pack ct), " argnames: ", concat $ map pack an, " argtypes: ", concat $ map (debugToBytestring . makeArgtype) at,"\n"]
makeCtor pname Constructor { what = Just ct, name = cn, argtypes = Just at, argnames = Nothing } =
    Right $ concat [" +++ ", (pack cn), " of type ", (pack ct), " argtypes: ",concat $ map (debugToBytestring . makeArgtype) at, "\n"]
makeCtor pname Constructor { what = Just ct, name = cn, argtypes = Nothing, argnames = Just an } =
    Right $ concat ["Constructor ", (pack cn), " of type ", (pack ct), " argnames: ", concat $ map pack an, "\n"]
makeCtor pname Constructor { what = Just ct, name = cn, argtypes = Nothing, argnames = Nothing } =
    Right $ concat ["Constructor ", (pack cn), " of type ", (pack ct), "\n"]
makeCtor pname Constructor { what = Nothing, name = cn, argtypes = Just at, argnames = Just an } =
    Right $ concat [" +++ ", (pack cn), " argtypes: ", concat $ map (debugToBytestring . makeArgtype) at, " argnames: ", concat $ map pack an, "\n"]
makeCtor pname Constructor { what = Nothing, name = cn, argtypes = Just at, argnames = Nothing } =
    Right $ concat ["function {:constructor} ", (pack . lowercase) cn, "`", pack (show $ length at), "(", concat $ map (debugToBytestring . makeArgtype) at, ")", "\n"]
makeCtor pname Constructor { what = Nothing, name = cn, argtypes = Nothing, argnames = Just an } =
    Right $ concat ["Constructor ", (pack cn), " argnames: ", concat $ map pack an, "\n"]
makeCtor pname Constructor { what = Nothing, name = cn, argtypes = Nothing, argnames = Nothing } =
    Right $ concat ["Constructor ", (pack cn), "\n"]

debugToBytestring :: Either String ByteString -> ByteString
debugToBytestring (Right bs) = concat $ [" {",bs, "} "]
debugToBytestring (Left s) = concat $ ["! ", pack s, " !"]

-- Argtype {what = "type:var", name = "T", args = Nothing},
makeArgtype :: Argtype -> Either String ByteString
makeArgtype Argtype { what = at, name = an, args = Just al } =
-- function {:constructor} node`3(value:TT, left: Tree, right: Tree) : Tree;
    Right $ concat $ [" name: ", pack $ an, " "] ++ map (debugToBytestring . makeArg) al
makeArgtype Argtype { what = at, name = an, args = Nothing } = --value:TT
    Right $ concat $ map pack [an, ":", at]

makeArg :: Arg -> Either String ByteString
makeArg Arg { what = at, name = String an} = Right $ concat $ map pack [show an, ":", at]
makeArg Arg { what = at, name = Number an} = Right $ concat $ map pack [show an, ":", at]
makeArg Arg { what = at, name = Bool an} = Right $ concat $ map pack [show an, ":", at]
makeArg Arg { what = at, name = Object an} = Right $ concat $ map pack [show an, ":", at]
makeArg Arg { what = at, name = Array an} = Right $ concat $ map pack [show an, ":", at]
makeArg Arg { what = _, name = an} = Left $ "Bad argument: " ++ show an

-- Parse JSON file into a Module
parse :: ByteString -> Either String Module
parse buffer = eitherDecode buffer :: Either String Module

