{-# LANGUAGE DeriveAnyClass, TemplateHaskell, RecordWildCards, DeriveGeneric, DuplicateRecordFields, OverloadedStrings  #-}
module Codegen.File where
import GHC.Generics
import Parser.Mod
import Data.Aeson
import Data.List (nub, sort)
import Codegen.Decl
import Common.Flatten
import Control.Lens
import Sema.Pipeline
import Data.Text (Text)

data CFile = CFile { _includes :: [Text], _decls :: [CDecl] }
    deriving (Eq, Generic, ToJSON)

makeLenses ''CFile

-- optimize pipeline
optimize :: CDecl -> CDecl
optimize = 
-- apply syntanctic optimizations
    over fbody (renames . semantics)
    . over ftype retypes

-- TODO: Ignore used modules for now
compile :: Module -> CFile
compile Module { .. } = CFile incls cdecls
    where cdecls = map (optimize . toCDecl) declarations
          incls = sort . nub . concat $ map getLibs cdecls

