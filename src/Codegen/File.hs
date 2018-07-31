{-# LANGUAGE DeriveAnyClass, TemplateHaskell, RecordWildCards, DeriveGeneric, DuplicateRecordFields, OverloadedStrings  #-}
module Codegen.File where
import GHC.Generics
import Parser.Mod
import Data.Aeson
import Data.List (nub)
import Codegen.Func
import Codegen.Defs
import Codegen.Utils
import Clang.Namespaces
import Control.Lens
import Data.Text (Text)
import qualified Data.Text as T

data CFile = CFile { _includes :: [Text], _filename :: Text, _funcs :: [CFunc] }
    deriving (Eq, Generic, ToJSON)

makeLenses ''CFile

getNativeLibs :: CFunc -> [Text]
getNativeLibs CFuncEmpty {} = []
getNativeLibs f = map normalizeType $ _ftype f : (map _typename (_fargs f))
    where normalizeType = T.replace "Datatypes." "" . T.toLower . removeRef . removeTemplate

-- JSON plugin doesnt export parametric types so have to do this hack
patchDecl :: CFunc -> CFunc
patchDecl = over ftype patch
    where patch "proc" = "void"
          patch o = o

-- TODO: Ignore used modules for now
compile :: Module -> CFile
compile Module { name = n, declarations = decls, .. } = CFile incls (T.append n ".cpp") cdecls
    where cdecls = map (patchDecl . toCDecl) decls
          incls = nub $ concat $ map (getNativeLibs . toCDecl) decls



