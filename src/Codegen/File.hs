{-# LANGUAGE DeriveAnyClass, TemplateHaskell, RecordWildCards, DeriveGeneric, DuplicateRecordFields, OverloadedStrings  #-}
module Codegen.File where
import GHC.Generics
import Parser.Mod
import Data.Aeson
import Data.List (nub)
import Codegen.Decl
import Codegen.Defs
import Codegen.Utils
import Codegen.Rewrite
import Control.Lens
import Sema.Pipeline
import Data.Text (Text)
import qualified Data.Text as T
import qualified Codegen.Config as Conf

data CFile = CFile { _includes :: [Text], _decls :: [CDecl] }
    deriving (Eq, Generic, ToJSON)

makeLenses ''CFile

getLibs :: CDecl -> [Text]
getLibs CEmpty {}     = []
getLibs CFunc  { .. } = filter (flip elem Conf.libs) $ (normalizeType _ftype):typargs
    where normalizeType = T.replace "Datatypes." "" . T.toLower . removeTemplate
          typargs = map (normalizeType . _typename) _fargs

-- optimize pipeline
optimize :: CDecl -> CDecl
optimize = over fbody (renames . semantics)

-- TODO: Ignore used modules for now
compile :: Module -> CFile
compile Module { .. } = CFile incls cdecls
    where cdecls = map (optimize . toCDecl) declarations
          incls = nub $ concat $ map (getLibs . toCDecl) declarations


