{-# LANGUAGE DeriveAnyClass, TemplateHaskell, RecordWildCards, DeriveGeneric, DuplicateRecordFields, OverloadedStrings  #-}
module Codegen.File where
import GHC.Generics
import Parser.Mod
import Data.Aeson
import Data.List (nub, sort)
import Codegen.Decl
import Codegen.Defs
import Codegen.Rewrite
import Common.Utils
import Common.Flatten
import Control.Lens
import Sema.Pipeline
import Data.Text (Text)
import qualified Common.Config as Conf
import qualified Data.Text as T

data CFile = CFile { _includes :: [Text], _decls :: [CDecl] }
    deriving (Eq, Generic, ToJSON)

makeLenses ''CFile

-- Traverse Declarations for libraries
getLibs :: CDecl -> [Text]
getLibs CEmpty {}     = []
getLibs CFunc  { .. } = nub . filter (flip elem Conf.libs) $ typargs ++ bodyargs
    where normalizeType = T.toLower . toCType . removeTemplate
          typargs = map (normalizeType . _typename) _fargs
          bodyargs = map normalizeType $ getTypes _fbody

-- optimize pipeline
optimize :: CDecl -> CDecl
optimize = over fbody (renames . semantics)

-- TODO: Ignore used modules for now
compile :: Module -> CFile
compile Module { .. } = CFile incls cdecls
    where cdecls = map (optimize . toCDecl) declarations
          incls = sort . nub . concat $ map getLibs cdecls

