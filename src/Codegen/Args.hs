{-# LANGUAGE DuplicateRecordFields, OverloadedStrings, TemplateHaskell #-}
module Codegen.Args (makeArgtype, makeArg) where
import Data.Aeson
import Codegen.Schema
import Utils (debugToBytestring)
import Prelude hiding (concat)
import Data.ByteString.Lazy.Char8 (ByteString, concat, pack, unpack)

-- Argtype {what = "type:var", name = "T", args = Nothing},
makeArgtype :: Argtype -> Either String ByteString
makeArgtype Argtype { what = at, name = an, args = Just al } =
    mconcat <$> mapM makeArg al
makeArgtype Argtype { what = at, name = an, args = Nothing } = --value:TT
    Right $ concat $ map pack [an, ":", at]

makeArg :: Arg -> Either String ByteString
makeArg Arg { what = at, name = String an} = Right $ concat $ map pack [show an, ":", at]
makeArg Arg { what = at, name = Number an} = Right $ concat $ map pack [show an, ":", at]
makeArg Arg { what = at, name = Bool an} = Right $ concat $ map pack [show an, ":", at]
makeArg Arg { what = at, name = Object an} = Right $ concat $ map pack [show an, ":", at]
makeArg Arg { what = at, name = Array an} = Right $ concat $ map pack [show an, ":", at]
makeArg Arg { what = _, name = an} = Left $ "Bad argument: " ++ show an



