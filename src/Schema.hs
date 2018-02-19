{-# LANGUAGE TemplateHaskell #-}
module Schema where
import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import Data.ByteString.Lazy.Char8 (ByteString)

---- From casting to Maybe Value

-- Object (fromList [("what",String "module"),("need_dummy",Bool False),("used_modules",Array [String "Datatypes"]),("need_magic",Bool False),("name",String "bt"),
--   ("declarations",
--      Array [Object (fromList
--        [("what",String "decl:ind"),("argnames",Array [String "T"]),
--           ("constructors",
--               Array [Object (fromList [("name",String "Leaf"),("argtypes",Array [])]),Object (fromList [("name",String "Node"),("argtypes",Array [Object (fromList [("args",Array [Object (fromList [("what",String "type:var"),("name",String "T")])]),("what",String "type:glob"),("name",String "tree")]),Object (fromList [("what",String "type:var"),("name",String "T")]),Object (fromList [("args",Array [Object (fromList [("what",String "type:var"),("name",String "T")])]),("what",String "type:glob"),("name",String "tree")])])])]),("name",String "tree")]),

--       Object (fromList [("what",String "decl:term"),("value",Object (fromList [("what",String "expr:lambda"),("argnames",Array [String "t"]),("body",Object (fromList [("cases",Array [Object (fromList [("what",String "case"),("pat",Object (fromList [("what",String "pat:constructor"),("argnames",Array []),("name",String "Leaf")])),("body",Object (fromList [("args",Array []),("what",String "expr:constructor"),("name",String "Leaf")]))]),Object (fromList [("what",String "case"),("pat",Object (fromList [("what",String "pat:constructor"),("argnames",Array [String "l",String "_",String "_"]),("name",String "Node")])),("body",Object (fromList [("what",String "expr:rel"),("name",String "l")]))])]),("expr",Object (fromList [("what",String "expr:rel"),("name",String "t")])),("what",String "expr:case")]))])),("name",String "get_left"),
-- ("type",Object (fromList [("what",String "type:arrow"),("left",Object (fromList [("args",Array [Object (fromList [("what",String "type:varidx"),

-- ("name",Number 1.0)])]),("what",String "type:glob"),("name",String "tree")])),("right",Object (fromList [("args",Array [Object (fromList [("what",String "type:varidx"),("name",Number 1.0)])]),("what",String "type:glob"),("name",String "tree")]))]))]),

--       Object (fromList [("what",String "decl:term"),("value",Object (fromList [("what",String "expr:lambda"),("argnames",Array [String "t"]),("body",Object (fromList [("cases",Array [Object (fromList [("what",String "case"),("pat",Object (fromList [("what",String "pat:constructor"),("argnames",Array []),("name",String "Leaf")])),("body",Object (fromList [("args",Array []),("what",String "expr:constructor"),("name",String "Datatypes.None")]))]),Object (fromList [("what",String "case"),("pat",Object (fromList [("what",String "pat:constructor"),("argnames",Array [String "_",String "v",String "_"]),("name",String "Node")])),("body",Object (fromList [("args",Array [Object (fromList [("what",String "expr:rel"),("name",String "v")])]),("what",String "expr:constructor"),("name",String "Datatypes.Some")]))])]),("expr",Object (fromList [("what",String "expr:rel"),("name",String "t")])),("what",String "expr:case")]))])),("name",String "get_val"),("type",Object (fromList [("what",String "type:arrow"),("left",Object (fromList [("args",Array [Object (fromList [("what",String "type:varidx"),("name",Number 1.0)])]),("what",String "type:glob"),("name",String "tree")])),("right",Object (fromList [("args",Array [Object (fromList [("what",String "type:varidx"),("name",Number 1.0)])]),("what",String "type:glob"),("name",String "Datatypes.option")]))]))])])])

---- From casting with Module schema
-- Module {what = "module", name = "bt", need_magic = False, need_dummy = False, used_modules = ["Datatypes"], declarations = [Declaration {what = "decl:ind", name = "tree", argnames = Just ["T"], typ = Nothing, constructors = Just [Constructor {what = Nothing, name = "Leaf", argtypes = Just [], argnames = Nothing},Constructor {what = Nothing, name = "Node", argtypes = Just [Argtype {what = "type:glob", name = "tree", args = Just [Arg {what = "type:var", name = String "T"}]},Argtype {what = "type:var", name = "T", args = Nothing},Argtype {what = "type:glob", name = "tree", args = Just [Arg {what = "type:var", name = String "T"}]}], argnames = Nothing}]},Declaration {what = "decl:term", name = "get_left", argnames = Nothing, typ = Just (Typ {what = "type:arrow", left = Argtype {what = "type:glob", name = "tree", args = Just [Arg {what = "type:varidx", name = Number 1.0}]}, right = Argtype {what = "type:glob", name = "tree", args = Just [Arg {what = "type:varidx", name = Number 1.0}]}}), constructors = Nothing},Declaration {what = "decl:term", name = "get_val", argnames = Nothing, typ = Just (Typ {what = "type:arrow", left = Argtype {what = "type:glob", name = "tree", args = Just [Arg {what = "type:varidx", name = Number 1.0}]}, right = Argtype {what = "type:glob", name = "Datatypes.option", args = Just [Arg {what = "type:varidx", name = Number 1.0}]}}), constructors = Nothing}]}

data Module = Module { what :: Text, name :: Text, need_magic :: Bool, need_dummy :: Bool,
                       used_modules :: [Text], declarations :: [Declaration] } deriving (Show, Eq)

data Declaration = Declaration { what :: Text, name :: Text, argnames :: Maybe [Text], typ :: Maybe Typ, constructors :: Maybe [Constructor] } deriving (Show, Eq)
data Typ = Typ { what :: Text, left :: Argtype, right :: Argtype } deriving (Show, Eq)
data Body = Body { what :: Text, expr :: Arg, cases :: [Cases] } deriving (Show, Eq)
data Cases = Cases { what :: Text, pat :: [Constructor], body :: Maybe Body } deriving (Show, Eq)
data Constructor = Constructor { what :: Maybe Text, name :: Text, argtypes :: Maybe [Argtype], argnames :: Maybe [Text] } deriving (Show, Eq)
data Argtype = Argtype { what :: Text, name :: Text, args :: Maybe [Arg] } deriving (Show, Eq)
data Arg = Arg { what :: Text, name :: Value } deriving (Show, Eq)

$(deriveJSON defaultOptions ''Module)
$(deriveJSON defaultOptions ''Declaration)
$(deriveJSON defaultOptions ''Constructor)
$(deriveJSON defaultOptions ''Typ)
$(deriveJSON defaultOptions ''Body)
$(deriveJSON defaultOptions ''Cases)
$(deriveJSON defaultOptions ''Argtype)
$(deriveJSON defaultOptions ''Arg)

parseModule :: ByteString -> Either String Module
parseModule buffer = eitherDecode buffer :: Either String Module

