{-# LANGUAGE DeriveAnyClass, TemplateHaskell, RecordWildCards, DeriveGeneric, DuplicateRecordFields, OverloadedStrings  #-}
module Codegen.File where
import GHC.Generics
import Parser.Mod
import Data.Aeson
import Data.List (nub)
import Codegen.Func
import Codegen.Defs
import Codegen.Utils
import Codegen.Config
import Clang.Namespaces
import Control.Lens
import Data.Text (Text)
import qualified Data.Text as T

data CFile = CFile { _includes :: [Text], _funcs :: [CFunc] }
    deriving (Eq, Generic, ToJSON)

makeLenses ''CFile

getNativeLibs :: CFunc -> [Text]
getNativeLibs CFuncEmpty {} = []
getNativeLibs f = filter (flip elem $ libs) $ (normalizeType . _ftype $ f):typargs
    where normalizeType = T.replace "Datatypes." "" . T.toLower . removeRef . removeTemplate
          typargs = map (normalizeType . _typename) $ _fargs f
-- TODO: Ignore used modules for now
compile :: Module -> CFile
compile Module { name = n, declarations = decls, .. } = CFile incls cdecls
    where cdecls = map toCDecl decls
          incls = nub $ concat $ map (getNativeLibs . toCDecl) decls



