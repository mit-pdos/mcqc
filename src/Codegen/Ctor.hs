{-# LANGUAGE DuplicateRecordFields, OverloadedStrings, TemplateHaskell #-}
module Codegen.Ctor (makeIndCtor) where
import Codegen.Args (makeArgtype, makeArg)
import Codegen.Schema
import Utils (debugToBytestring, lowercase)
import Prelude hiding (concat)
import Data.ByteString.Lazy.Char8 (ByteString, concat, pack, unpack)

-- Coq to C++ constructors
makeIndCtor :: String -> Constructor -> Either String ByteString
makeIndCtor pname Constructor { what = Just ct, name = cn, argtypes = Just at, argnames = Just an } =
    Right $ concat [" +++ ", (pack cn), " of type ", (pack ct), " argnames: ", concat $ map pack an, " argtypes: ", concat $ map (debugToBytestring . makeArgtype) at,"\n"]
makeIndCtor pname Constructor { what = Just ct, name = cn, argtypes = Just at, argnames = Nothing } =
    Right $ concat [" +++ ", (pack cn), " of type ", (pack ct), " argtypes: ",concat $ map (debugToBytestring . makeArgtype) at, "\n"]
makeIndCtor pname Constructor { what = Just ct, name = cn, argtypes = Nothing, argnames = Just an } =
    Right $ concat ["Constructor ", (pack cn), " of type ", (pack ct), " argnames: ", concat $ map pack an, "\n"]
makeIndCtor pname Constructor { what = Just ct, name = cn, argtypes = Nothing, argnames = Nothing } =
    Right $ concat ["Constructor ", (pack cn), " of type ", (pack ct), "\n"]
makeIndCtor pname Constructor { what = Nothing, name = cn, argtypes = Just at, argnames = Just an } =
    Right $ concat [" +++ ", (pack cn), " argtypes: ", concat $ map (debugToBytestring . makeArgtype) at, " argnames: ", concat $ map pack an, "\n"]
makeIndCtor pname Constructor { what = Nothing, name = cn, argtypes = Just at, argnames = Nothing } =
    Right $ concat ["function {:constructor} ", (pack . lowercase) cn, "`", pack (show $ length at), "(", concat $ map (debugToBytestring . makeArgtype) at, ")", "\n"]
makeIndCtor pname Constructor { what = Nothing, name = cn, argtypes = Nothing, argnames = Just an } =
    Right $ concat ["Constructor ", (pack cn), " argnames: ", concat $ map pack an, "\n"]
makeIndCtor pname Constructor { what = Nothing, name = cn, argtypes = Nothing, argnames = Nothing } =
    Right $ concat ["Constructor ", (pack cn), "\n"]


